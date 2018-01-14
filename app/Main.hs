{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil.Parser
import Pencil.Env
import Control.Exception (tryJust)
import Control.Monad (forM_, foldM, filterM, liftM, (>=>))
import GHC.IO.Exception (IOException(ioe_description))
import Data.Typeable     ( Typeable )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Text.Pandoc as P
import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.HTML.TagSoup as TS
import qualified Text.Sass as Sass
import Text.Sass.Options (defaultSassOptions)

type PTags = [PTag]

sitePrefix :: String
sitePrefix = "site/"

outPrefix :: String
outPrefix = "out/"

globalEnv :: Env
globalEnv = H.fromList [("title", EText "Elben Shira's Awesome Website")]

-- | Apply the environment variables on the given pages.
--
-- The NonEmpty is expected to be ordered by inner-most content first (such that
-- the final, HTML structure layout is last in the list).
--
-- The returned Page contains the Nodes of the fully rendered page, the
-- fully-applied environment, and the URL of the last (inner-most) Page.
--
-- The variable application works by applying the outer environments down into
-- the inner environments, until it hits the lowest environment, in which the
-- page is rendered. Once done, this rendered content is saved as the ${body}
-- variable for the parent structure, which is then applied, and so on.
--
-- As an example, there is the common scenario where we have a default layout
-- (e.g. "default.html"), with the full HTML structure, but no body. It has only
-- a "${body}" template variable inside. This is the parent layout. There is a
-- child layout, the partial called "blog-post.html", which has HTML for
-- rendering a blog post, like usage of ${postTitle} and ${postDate}. Inside
-- this, there is another child layout, the blog post content itself, which
-- defines the variables $postTitle and $postDate, and may renderer parent
-- variables such as ${websiteTitle}.
--
--   +--------------+
--   |              | <--- default.html
--   |              |      Defines ${websiteTitle}
--   |  +---------+ |
--   |  |         |<+----- blog-post.html
--   |  | +-----+ | |      Renders ${postTitle}, ${postDate}
--   |  | |     | | |
--   |  | |     | | |
--   |  | |     |<+-+----- blog-article-content.markdown
--   |  | |     | | |      Renders ${websiteTitle}
--   |  | +-----+ | |      Defines ${postTitle}, ${postDate}
--   |  +---------+ |
--   +--------------+
--
-- In this case, we want to accumulate the environment variables, starting from
-- default.html, to blog-post.html, and the markdown file's variables. Combine
-- all of that, then render the blog post content. This content is then injected
-- into the parent's environment as a $body variable, for use in blog-post.html.
-- Now *that* content is injected into the parent environment's $body variable,
-- which is then used to render the full-blown HTML page.
--
applyPage :: Env -> NonEmpty Page -> IO Page
applyPage env pages = applyPage' env (NE.reverse pages)

-- It's simpler to implement if NonEmpty is ordered outer-structure first (e.g.
-- HTML layout).
applyPage' :: Env -> NonEmpty Page -> IO Page
applyPage' env (Page nodes penv fp :| []) = do
  let env' = H.union penv env -- LHS overrides RHS
  nodes' <- evalNodes env' nodes
  return $ Page nodes' env' fp
applyPage' env (Page nodes penv _ :| (headp : rest)) = do
  Page nodes' env' fpInner <- applyPage' (H.union penv env) (headp :| rest)
  let env'' = H.insert "body" (EText (renderNodes nodes')) env'
  nodes'' <- evalNodes env'' nodes
  -- Get the inner-most Page's file path, and pass that upwards to the returned
  -- Page.
  return $ Page nodes'' env'' fpInner

-- | Rewrite file path for blog posts.
-- "/blog/2011-01-01-the-post-title.html" => "/blog/the-post-title/"
blogPostUrl :: FilePath -> FilePath
blogPostUrl fp = FP.replaceFileName fp (drop 11 (FP.takeBaseName fp)) ++ "/"

-- | Modify a variable in the env.
modifyEnvVar :: Page -> (EnvData -> EnvData) -> T.Text -> Page
modifyEnvVar (Page nodes env fp) f k =
  let env' = H.adjust f k env
  in Page nodes env' fp

prepareBlogPost :: H.HashMap T.Text Page -> Page -> Page
prepareBlogPost tagMap page@(Page _ env _) =
  let tagEnvList =
        case H.lookup "tags" env of
          Just (EArray tags) ->
            EEnvList $
              L.foldl'
                (\acc envData ->
                  case envData of
                    EText tag ->
                      case H.lookup tag tagMap of
                        Just tagIndexPage -> getPageEnv tagIndexPage : acc
                        _ -> acc
                    _ -> acc)
                [] tags
          _ -> EEnvList []

      -- Overwrite the EArray "tags" variable in the post Page with EEnvList of the
      -- loaded Tag index pages. This is so that when we render the blog posts, we
      -- have access to the URL of the Tag index.
      env' = H.insert "tags" tagEnvList env
  in page { getPageEnv = env' }

loadAndApplyPage :: NonEmpty Page -> FilePath -> IO ()
loadAndApplyPage structure fp = do
  page <- liftM forceRight (loadPageAsHtml fp)
  applyPage globalEnv (NE.cons page structure) >>= renderPage

sortByVar :: T.Text
          -- ^ Variable name to look up in Env.
          -> (EnvData -> EnvData -> Ordering)
          -- ^ Ordering function to compare EnvData against. If the variable is
          -- not in the Env, the Page will be placed at the bottom of the order.
          -> [Resource]
          -> [Resource]
sortByVar var ordering =
  L.sortBy
    (\ra rb ->
      case (ra, rb) of
        (Single (Page _ enva _), Single (Page _ envb _)) ->
          maybeOrdering ordering (H.lookup var enva) (H.lookup var envb)
        _ -> EQ)

sortPagesByVar :: T.Text
          -- ^ Variable name to look up in Env.
          -> (EnvData -> EnvData -> Ordering)
          -- ^ Ordering function to compare EnvData against. If the variable is
          -- not in the Env, the Page will be placed at the bottom of the order.
          -> [Page]
          -> [Page]
sortPagesByVar var ordering =
  L.sortBy
    (\(Page _ enva _) (Page _ envb _) ->
      maybeOrdering ordering (H.lookup var enva) (H.lookup var envb))

-- | Filter by a variable's value in the environment. If missing, it is not
-- included in the final result.
filterByVar :: T.Text
            -> (EnvData -> Bool)
            -> [Page]
            -> [Page]
filterByVar var f =
  L.filter
   (\(Page _ env _) -> M.fromMaybe False (H.lookup var env >>= (Just . f)))

-- | Given a variable (whose value is assumed to be an array of EText) and list
-- of pages, group the pages by the EText found in the variable.
--
-- For example, say each Page has a variable "tags" that is a list of tags. The
-- first Page has a "tags" variable that is an EArray [EText "a"], and the
-- second Page has a "tags" variable that is an EArray [EText "a", EText "b"].
-- The final output would be a map fromList [("a", [page1, page2]), ("b",
-- [page2])].
groupByTagVar :: T.Text
           -> [Page]
           -> H.HashMap T.Text [Page]
groupByTagVar var =
  -- This outer fold takes the list of pages, and accumulates the giant HashMap.
  L.foldl'
    (\acc page@(Page _ env _) ->
      let x = H.lookup var env
      in case x of
           Just (EArray values) ->
             -- This fold takes each of the found values (each is a key in the
             -- hash map), and adds the current page (from the outer fold) into
             -- each of the key.
             L.foldl'
               (\hashmap envData ->
                 case envData of
                   -- Only insert Pages into the map if the variable is an EArray of
                   -- EText. Alter the map to either (1) insert this current
                   -- page into the existing list, or (2) create a new list (the
                   -- key has never been seen) with just this page.
                   EText val -> H.alter (\mv -> Just (page : M.fromMaybe [] mv)) val hashmap
                   _ -> hashmap)
               acc values
           _ -> acc
    )
    H.empty

forceRight :: Show e => Either e a -> a
forceRight (Right a) = a
forceRight (Left e) = error (show e)

main :: IO ()
main = do
  pageLayout <- liftM forceRight (loadPageAsHtml "layouts/default.html")
  pagePartial <- liftM forceRight (loadPageAsHtml "partials/post.html")

  -- Load posts
  postFps <- listDir False "blog/"
  posts <- mapM
    (liftM forceRight . loadPageWithFileModifier blogPostUrl)
    postFps

  let sortedPosts = sortPagesByVar "date" dateOrdering posts
  let recommendedPosts = filterByVar "tags"
                           (arrayContainsString "recommended")
                           posts

  -- Tags and tag list pages

  let tagMap = groupByTagVar "tags" posts
  -- Build a mapping of tag to the tag list Page
  tagPages <- foldM
    (\acc (tag, taggedPosts) -> do
      tagPage <- liftM forceRight $ loadPageWithFileModifier (const ("blog/tags/" ++ T.unpack tag ++ "/")) "partials/post-list-for-tag.html"
      let tagEnv = (insertEnvListPage "posts" taggedPosts . insertEnvText "tag" tag . insertEnv (getPageEnv tagPage)) globalEnv
      return $ H.insert tag (tagPage { getPageEnv = tagEnv }) acc
    )
    H.empty
    (H.toList tagMap)

  -- Prepare blog posts. Add tag info into each blog post page, and then inject
  -- into the correct structure.
  let posts' = map (structurePage (pagePartial :| [pageLayout]) . prepareBlogPost tagPages) posts

  -- Render blog posts
  forM_ posts' (applyPage globalEnv >=> renderPage)

  -- Index
  -- Function composition
  let postsEnv = (insertEnvListPage "posts" sortedPosts . insertEnvListPage "recommendedPosts" recommendedPosts) globalEnv
  indexPage <- liftM forceRight $ loadPageAsHtml "index.html"
  applyPage postsEnv (indexPage :| [pageLayout]) >>= renderPage

  -- Render tag list pages
  forM_ (H.elems tagPages) (\page -> applyPage globalEnv (page :| [pageLayout]) >>= renderPage)

  -- Render blog post archive
  archivePage <- liftM forceRight $ loadPageWithFileModifier (const "blog/") "partials/post-archive.html"
  let postsArchiveEnv = insertEnvListPage "posts" sortedPosts globalEnv
  applyPage postsArchiveEnv (archivePage :| [pageLayout]) >>= renderPage

  -- Render CSS file
  renderCss "stylesheets/default.scss"

  -- Render /p/ mini sites
  loadDir True True "p/" >>= renderResources

loadDir :: Bool -> Bool -> FilePath -> IO [Resource]
loadDir recursive strict dir = do
  fps <- listDir recursive dir
  if strict
    then return $ map (\fp -> Passthrough fp fp) fps
    else mapM loadResourceId fps

-- List files in directory, optionally recursively. Returns paths that include
-- the given dir.
listDir :: Bool -> FilePath -> IO [FilePath]
listDir recursive dir = do
  fps <- listDir' recursive dir
  return $ map (dir ++) fps

listDir' :: Bool -> FilePath -> IO [FilePath]
listDir' recursive dir = do
  -- List files (just the filename, without the fp directory prefix)
  listing <- D.listDirectory (sitePrefix ++ dir)
  -- Filter only for files (we have to add the right directory prefixes to the
  -- file check)
  files <- filterM (\f -> D.doesFileExist (sitePrefix ++ dir ++ f)) listing
  dirs <- filterM (\f -> D.doesDirectoryExist (sitePrefix ++ dir ++ f)) listing

  innerFiles <- if recursive
                  then mapM
                         (\d -> do
                           ff <- listDir' recursive (dir ++ d ++ "/")
                           -- Add the inner directory as a prefix
                           return (map (\f -> d ++ "/" ++ f) ff))
                         dirs
                  else return []

  return $ files ++ concat innerFiles

insertEnvText :: T.Text -> T.Text -> Env -> Env
insertEnvText var val = H.insert var (EText val)

insertEnvListPage :: T.Text -> [Page] -> Env -> Env
insertEnvListPage var posts = H.insert var (EEnvList (map getPageEnv posts))

-- insertEnvList :: T.Text -> [Resource] -> Env -> Env
-- insertEnvList var posts = H.insert var (EEnvList (map getPageEnv posts))

-- | Merge two envs together, biased towards the left-hand env on duplicates.
insertEnv :: Env -> Env -> Env
insertEnv = H.union

parseMaybeText :: T.Text -> A.Object -> Maybe T.Text
parseMaybeText k = parseMaybe (\o -> o A..: k :: Parser T.Text)

-- | Convert known Aeson types into known Env types.
maybeInsertIntoEnv :: Env -> T.Text -> A.Value -> Env
maybeInsertIntoEnv env k v =
  case toEnvData v of
    Nothing -> env
    Just d -> H.insert k d env

-- | Convert an Aeson Object to an Env.
aesonToEnv :: A.Object -> Env
aesonToEnv = H.foldlWithKey' maybeInsertIntoEnv H.empty

-- Describes a loaded page, with the page's template nodes, loaded environment
-- from the preamble, and where the page was loaded from.
data Page = Page
  { getPageNodes     :: [PNode]
  , getPageEnv       :: Env
  , getPageFilePath  :: String
  -- ^ The rendered output path of this page. Defaults to the input file path.
  -- This file path is used to generate the self URL that is injected into the
  -- environment.
  } deriving (Eq, Show)

data Resource
  = Single Page
  | Passthrough FilePath FilePath
  -- ^ in and out file paths

renderResource :: Resource -> IO ()
renderResource (Single page) = renderPage page
renderResource (Passthrough fpIn fpOut) = copyFile fpIn fpOut

renderResources :: [Resource] -> IO ()
renderResources resources = forM_ resources renderResource

structurePage :: NonEmpty Page -> Page -> NonEmpty Page
structurePage structure page = NE.cons page structure

copyFile :: FilePath -> FilePath -> IO ()
copyFile fpIn fpOut = do
  D.createDirectoryIfMissing True (FP.takeDirectory (outPrefix ++ fpOut))
  D.copyFile (sitePrefix ++ fpIn) (outPrefix ++ fpOut)


loadResourceAsHtml :: FilePath -> IO Resource
loadResourceAsHtml = loadResourceWithFileModifier (\fp -> FP.dropExtension fp ++ ".html")

loadResourceId :: FilePath -> IO Resource
loadResourceId = loadResourceWithFileModifier id

loadResourceWithFileModifier :: (FilePath -> FilePath) -> FilePath -> IO Resource
loadResourceWithFileModifier fpf fp = do
  eitherPage <- loadPageWithFileModifier fpf fp
  case eitherPage of
    Left (NotTextFile _) -> return $ Passthrough fp (fpf fp)
    Right page -> return (Single page)

renderPage :: Page -> IO ()
renderPage (Page nodes _ fpOut) = do
  let noFileName = FP.takeBaseName fpOut == ""
  let fpOut' = outPrefix ++ if noFileName then fpOut ++ "index.html" else fpOut
  D.createDirectoryIfMissing True (FP.takeDirectory fpOut')
  TIO.writeFile fpOut' (renderNodes nodes)

-- | Load page, extracting the tags and preamble variables. Renders Markdown
-- files into HTML. Defaults the page output file path to the given input file
-- path, but as a folder, with an HTML extension.
--
-- As an example, if the given fp is "/foo/bar/hello.markdown", the returned
-- filepath is "/foo/bar/hello.html".
loadPageAsHtml :: FilePath
         -> IO (Either LoadFileException Page)
loadPageAsHtml = loadPageWithFileModifier (\fp -> FP.dropExtension fp ++ ".html")

loadPageId :: FilePath -> IO (Either LoadFileException Page)
loadPageId = loadPageWithFileModifier id

loadPageWithFileModifier :: (FilePath -> FilePath) -> FilePath -> IO (Either LoadFileException Page)
loadPageWithFileModifier fpf fp = do
  eitherContent <- parseTextFile fp
  case eitherContent of
    Left e -> return $ Left e
    Right (content, nodes) -> do
      let env = aesonToEnv $ loadVariables (TS.parseTags content)
      let fp' = "/" ++ fpf fp
      let env' = H.insert "this.url" (EText (T.pack fp')) env
      return $ Right $ Page nodes env' ("/" ++ fpf fp)

data Extension = Markdown
               | Sass
               | Other

extensionMap :: H.HashMap String Extension
extensionMap = H.fromList
  [ ("markdown", Markdown)
  , ("md", Markdown)
  , ("sass", Sass)
  , ("scss", Sass)]

toExtension :: String -> Extension
toExtension fp =
  -- takeExtension returns ".markdown", so drop the "."
  M.fromMaybe Other (H.lookup (drop 1 (FP.takeExtension fp)) extensionMap)

sassOptions :: Sass.SassOptions
sassOptions = Text.Sass.Options.defaultSassOptions

markdownWriterOptions :: P.WriterOptions
markdownWriterOptions =
  P.def {
    P.writerHighlight = True
  }

data LoadFileException
  = NotTextFile IOError
  -- ^ Failed to read a file as a text file.
  deriving (Typeable, Show)

loadTextFile :: FilePath -> IO (Either LoadFileException T.Text)
loadTextFile fp =
  tryJust toLoadFileException
          (TIO.readFile (sitePrefix ++ fp))

toLoadFileException :: IOError -> Maybe LoadFileException
toLoadFileException e = if isInvalidByteSequence e then Just (NotTextFile e) else Nothing

isInvalidByteSequence :: IOError -> Bool
isInvalidByteSequence e = ioe_description e == "invalid byte sequence"

parseTextFile :: FilePath -> IO (Either LoadFileException (T.Text, [PNode]))
parseTextFile fp = do
  eitherContent <- loadTextFile fp
  case eitherContent of
    Left e -> return $ Left e
    Right content -> do
      content' <-
        case toExtension fp of
          Markdown ->
            case P.readMarkdown P.def (T.unpack content) of
              Left _ -> return content
              Right pandoc -> return $ T.pack $ P.writeHtmlString markdownWriterOptions pandoc
          Sass -> do
             -- Use compileFile so that SASS @import works
             result <- Sass.compileFile (sitePrefix ++ fp) sassOptions
             case result of
               Left _ -> return content
               Right byteStr -> return $ decodeUtf8 byteStr
          Other -> return content
      let nodes = case runParser content' of
                    Left _ -> []
                    Right n -> n
      return $ Right (content', nodes)

-- | Evaluate the nodes in the given environment. Note that it returns an IO
-- because of ${partial(..)} calls that requires us to load a file.
evalNodes :: Env -> [PNode] -> IO [PNode]
evalNodes _ [] = return []
evalNodes env (PVar var : rest) = do
  nodes <- evalNodes env rest
  case H.lookup var env of
    Nothing -> return $ PVar var : nodes
    -- TODO for date rendering, we want to choose here HOW we want to render,
    -- given what? a type? Or a var name + type? Or should be have injected it
    -- into the env beforehand?
    Just envData -> return $ PText (envDataToDisplay envData) : nodes
evalNodes env (PIf var nodes : rest) = do
  rest' <- evalNodes env rest
  case H.lookup var env of
    -- Can't find var in env; Everything inside the if-statement is thrown away
    Nothing -> return rest'
    -- Render nodes inside the if-statement
    Just _ -> do
      nodes' <- evalNodes env nodes
      return $ nodes' ++ rest'
evalNodes env (PFor var nodes : rest) = do
  rest' <- evalNodes env rest
  case H.lookup var env of
    -- Can't find var in env; everything inside the for-statement is thrown away
    Nothing -> return rest'
    -- Render nodes inside the for-statement
    Just (EEnvList envs) -> do
      -- Render the for nodes once for each given env, and append them together
      forNodes <-
        foldM
          (\accNodes e -> do
              nodes' <- evalNodes (H.union e env) nodes
              return $ accNodes ++ nodes')
          [] envs
      return $ forNodes ++ rest'
    -- Var is not an EEnvList; everything inside the for-statement is thrown away
    Just _ -> return rest'
evalNodes env (PPartial fp : rest) = do
  (_, nodes) <- liftM forceRight $ parseTextFile (T.unpack fp)
  nodes' <- evalNodes env nodes
  rest' <- evalNodes env rest
  return $ nodes' ++ rest'
evalNodes env (n : rest) = do
  rest' <- evalNodes env rest
  return $ n : rest'

-- Find the PREAMBLE JSON section, parse it, and return as an Aeson Object.
loadVariables :: Tags -> A.Object
loadVariables tags =
  case findPreambleComment tags of
    Nothing -> H.empty
    Just commentText ->
      let v = A.decode (fromStrict $ encodeUtf8 (T.strip commentText)) :: Maybe A.Object
       in M.fromMaybe H.empty v

findPreambleComment :: Tags -> Maybe T.Text
findPreambleComment [] = Nothing
findPreambleComment (TS.TagComment str : rest) =
  if T.isPrefixOf "PREAMBLE" (T.strip str)
     then let (_, b) = T.breakOn "PREAMBLE" str
           in Just $ T.replace "PREAMBLE" "" b
  else findPreambleComment rest
findPreambleComment (_ : rest) =
  findPreambleComment rest

-- | Copy specified file from site to out.
renderCss :: FilePath -> IO ()
renderCss fp = do
  -- Drop .scss/sass extension and replace with .css.
  eitherPage <- loadPageWithFileModifier (\fp -> FP.dropExtension fp ++ ".css") fp
  case eitherPage of
    Right page -> renderPage page
    Left _ -> return ()

cssTag :: T.Text -> Tags
cssTag file = TS.parseTags $ T.append "<link rel=\"stylesheet\" href=\"" $ T.append file "\" />"

-- Inject tags into the body tag, at the ${body} annotation location.
injectIntoBodyVar :: Tags -> Tags -> Tags
injectIntoBodyVar _ [] = []
injectIntoBodyVar inject (tag @ (TS.TagText str) : rest) =
  if T.isInfixOf "${body}" str
  then
    -- Found the body var. Replace with the injection.
    let (a, b) = T.breakOn "${body}" str
    in TS.TagText a : inject ++ (TS.TagText (T.replace "${body}" "" b) : rest)
  else tag : injectIntoBodyVar inject rest
injectIntoBodyVar inject (tag : rest) =
  tag : injectIntoBodyVar inject rest

-- Inject the first set of tags into the template (second argument) right after
-- the body tag.
injectIntoBody :: Tags -> Tags -> Tags
injectIntoBody = injectInto "body"

-- | Inject tags into the HTML <head> element.
injectIntoHead :: Tags -- Tags to inject
               -> Tags -- Tags with <head> being injected into
               -> Tags -- Resulting tags
injectIntoHead = injectInto "head"

injectInto :: T.Text -> Tags -> Tags -> Tags
injectInto _ _ [] = []
injectInto tagName inject (tagOpen @ (TS.TagOpen tag _) : rest) =
  if tag == tagName
  then tagOpen : inject ++ rest
  else tagOpen : injectInto tagName inject rest
injectInto tagName inject (tag : rest) =
  tag : injectInto tagName inject rest
