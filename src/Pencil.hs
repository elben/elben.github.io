{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Pencil where

import Pencil.Env
import Pencil.Parser

import Control.Exception (tryJust)
import Control.Monad (forM_, foldM, filterM, liftM)
import Control.Monad.Reader
import Control.Monad.Except
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Typeable (Typeable)
import GHC.IO.Exception (IOException(ioe_description, ioe_filename, ioe_type), IOErrorType(NoSuchThing))
import Text.Sass.Options (defaultSassOptions)
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as A
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.Pandoc as P
import qualified Text.Sass as Sass

-- PencilApp a = Config -> IO (Except PencilException a)
--
-- Allows us to catch "checked" exceptions; errors that we know how to handle,
-- in PencilException.
--
-- Unknown "unchecked" exceptions can still go through IO.
type PencilApp = ReaderT Config (ExceptT PencilException IO)

data Config =
  Config
  { cSitePrefix :: String
  , cOutPrefix :: String
  , cEnv :: Env
  }

-- | Run the Pencil app.
--
-- Note that this can throw a fatal exception.
run :: PencilApp a -> Config -> IO ()
run app config = do
  e <- runExceptT $ runReaderT app config
  case e of
    Left err -> do
      putStrLn $ "Pencil got exception: " ++ show err
      case err of
        FileNotFound mfp ->
          case mfp of
            Just fp -> do
              e2 <- runExceptT $ runReaderT (mostSimilarFile fp) config
              case e2 of
                Left _ -> return ()
                Right mBest ->
                  case mBest of
                    Just best -> putStrLn ("Maybe you mean this: " ++ best)
                    Nothing -> return ()
            Nothing -> return ()
        _ -> return ()
    Right _ -> return ()

-- | Given a file path, look at all file paths and find the one that seems most
-- similar.
mostSimilarFile :: FilePath -> PencilApp (Maybe FilePath)
mostSimilarFile fp = do
  sitePrefix <- asks cSitePrefix
  fps <- listDir True ""
  let fps' = map (sitePrefix ++) fps -- add site prefix for distance search
  let costs = map (\f -> (f, levenshteinDistance defaultEditCosts fp f)) fps'
  let sorted = L.sortBy (\(_, d1) (_, d2) -> compare d1 d2) costs
  return $ fst <$> M.listToMaybe sorted

data PencilException
  = NotTextFile IOError
  -- ^ Failed to read a file as a text file.
  | FileNotFound (Maybe FilePath)
  -- ^ File not found. We may or may not know the file we were looking for.
  deriving (Typeable, Show)

data FileType = Markdown
              | Sass
              | Html
              | Other

extensionMap :: H.HashMap String FileType
extensionMap = H.fromList
  [ ("markdown", Markdown)
  , ("md", Markdown)
  , ("html", Html)
  , ("sass", Sass)
  , ("scss", Sass)]

toExtension :: FilePath -> FileType
toExtension fp =
  -- takeExtension returns ".markdown", so drop the "."
  M.fromMaybe Other (H.lookup (map toLower (drop 1 (FP.takeExtension fp))) extensionMap)

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
apply :: Env -> NonEmpty Page -> PencilApp Page
apply env pages = applyPage' env (NE.reverse pages)

-- It's simpler to implement if NonEmpty is ordered outer-structure first (e.g.
-- HTML layout).
applyPage' :: Env -> NonEmpty Page -> PencilApp Page
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

loadTextFile :: FilePath -> PencilApp T.Text
loadTextFile fp = do
  sitePrefix <- asks cSitePrefix
  -- Try to read the file. If it fails because it's not a text file, capture the
  -- exception and convert it to a "checked" exception in the ExceptT stack via
  -- 'throwError'.
  eitherContent <- liftIO $ tryJust toNotTextFileException (TIO.readFile (sitePrefix ++ fp))
  case eitherContent of
    Left e -> throwError e
    Right a -> return a

justLeft :: Either e a -> e
justLeft (Left e) = e
justLeft _ = error "not right"

-- How to test errors:
--
-- import Control.Exception
-- import qualified Data.Text.IO as TIO
--
-- (\e -> print (ioe_description (e :: IOError)) >> return "") `handle` (TIO.readFile "foo")
toNotTextFileException :: IOError -> Maybe PencilException
toNotTextFileException e
  | isInvalidByteSequence e = Just (NotTextFile e)
  | isNoSuchFile e = Just (FileNotFound (ioe_filename e))
  | otherwise = Nothing

isInvalidByteSequence :: IOError -> Bool
isInvalidByteSequence e = ioe_description e == "invalid byte sequence"

isNoSuchFile :: IOError -> Bool
isNoSuchFile e = ioe_type e == NoSuchThing

sassOptions :: Sass.SassOptions
sassOptions = Text.Sass.Options.defaultSassOptions

markdownWriterOptions :: P.WriterOptions
markdownWriterOptions =
  P.def {
    P.writerHighlight = True
  }

parseTextFile :: FilePath -> PencilApp (T.Text, [PNode])
parseTextFile fp = do
  content <- loadTextFile fp
  content' <-
    case toExtension fp of
      Markdown ->
        case P.readMarkdown P.def (T.unpack content) of
          Left _ -> return content
          Right pandoc -> return $ T.pack $ P.writeHtmlString markdownWriterOptions pandoc
      Sass -> do
        sitePrefix <- asks cSitePrefix
        -- Use compileFile so that SASS @import works
        result <- liftIO $ Sass.compileFile (sitePrefix ++ fp) sassOptions
        case result of
          Left _ -> return content
          Right byteStr -> return $ decodeUtf8 byteStr
      _ -> return content
  let nodes = case parseText content' of
                Left _ -> []
                Right n -> n
  return (content', nodes)

-- | Evaluate the nodes in the given environment. Note that it returns an IO
-- because of ${partial(..)} calls that requires us to load a file.
evalNodes :: Env -> [PNode] -> PencilApp [PNode]
evalNodes _ [] = return []
evalNodes env (PVar var : rest) = do
  nodes <- evalNodes env rest
  case H.lookup var env of
    Nothing -> return $ PVar var : nodes
    -- TODO for date rendering, we want to choose here HOW we want to render,
    -- given what? a type? Or a var name + type? Or should be have injected it
    -- into the env beforehand?
    Just envData -> return $ PText (toText envData) : nodes
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
  (_, nodes) <- parseTextFile (T.unpack fp)
  nodes' <- evalNodes env nodes
  rest' <- evalNodes env rest
  return $ nodes' ++ rest'
evalNodes env (n : rest) = do
  rest' <- evalNodes env rest
  return $ n : rest'

-- | Modify a variable in the env.
modifyEnvVar :: Page -> (EnvData -> EnvData) -> T.Text -> Page
modifyEnvVar (Page nodes env fp) f k =
  let env' = H.adjust f k env
  in Page nodes env' fp

loadAndApplyPage :: NonEmpty Page -> FilePath -> PencilApp ()
loadAndApplyPage structure fp = do
  env <- asks cEnv
  page <- loadHtml fp
  apply env (NE.cons page structure) >>= render

sortByVar :: T.Text
          -- ^ Variable name to look up in Env.
          -> (EnvData -> EnvData -> Ordering)
          -- ^ Ordering function to compare EnvData against. If the variable is
          -- not in the Env, the Page will be placed at the bottom of the order.
          -> [Page]
          -> [Page]
sortByVar var ordering =
  L.sortBy
    (\(Page _ enva _) (Page _ envb _) ->
      maybeOrdering ordering (H.lookup var enva) (H.lookup var envb))

-- | Filter by a variable's value in the environment.
filterByVar :: Bool
            -- ^ If true, include pages without the specified variable.
            -> T.Text
            -> (EnvData -> Bool)
            -> [Page]
            -> [Page]
filterByVar includeMissing var f =
  L.filter
   (\(Page _ env _) -> M.fromMaybe includeMissing (H.lookup var env >>= (Just . f)))

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
groupByTagVar var pages =
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
    (reverse pages)
    -- ^ Reverse to keep ordering consistent inside hash map, since the fold
    -- prepends into accumulated list.

-- | A file modifier that renames to HTML files that will be rendered to HTML.
fileModifierToHtml :: FilePath -> FilePath
fileModifierToHtml fp =
  case toExtension fp of
    Markdown -> FP.dropExtension fp ++ ".html"
    _ -> fp

loadDirId :: Bool -> Bool -> FilePath -> PencilApp [Resource]
loadDirId recursive strict = loadDirWithFileModifier recursive strict id

-- | Load directory as Resources.
loadDirWithFileModifier :: Bool -> Bool -> (FilePath -> FilePath) -> FilePath -> PencilApp [Resource]
loadDirWithFileModifier recursive strict fpf dir = do
  fps <- listDir recursive dir
  if strict
    then return $ map (\fp -> Passthrough fp fp) fps
    else mapM (loadResourceWithFileModifier fpf) fps

-- List files in directory, optionally recursively. Returns paths that include
-- the given dir.
listDir :: Bool -> FilePath -> PencilApp [FilePath]
listDir recursive dir = do
  let dir' = FP.addTrailingPathSeparator dir
  fps <- listDir' recursive dir'
  return $ map (dir' ++) fps

listDir' :: Bool -> FilePath -> PencilApp [FilePath]
listDir' recursive dir = do
  sitePrefix <- asks cSitePrefix
  -- List files (just the filename, without the fp directory prefix)
  listing <- liftIO $ D.listDirectory (sitePrefix ++ dir)
  -- Filter only for files (we have to add the right directory prefixes to the
  -- file check)
  files <- liftIO $ filterM (\f -> D.doesFileExist (sitePrefix ++ dir ++ f)) listing
  dirs <- liftIO $ filterM (\f -> D.doesDirectoryExist (sitePrefix ++ dir ++ f)) listing

  innerFiles <- if recursive
                  then mapM
                         (\d -> do
                           ff <- listDir' recursive (dir ++ d ++ "/")
                           -- Add the inner directory as a prefix
                           return (map (\f -> d ++ "/" ++ f) ff))
                         dirs
                  else return []

  return $ files ++ concat innerFiles

insertEnvData :: T.Text -> EnvData -> Env -> Env
insertEnvData = H.insert

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
parseMaybeText k = A.parseMaybe (\o -> o A..: k :: A.Parser T.Text)

-- | Convert known Aeson types into known Env types.
maybeInsertIntoEnv :: Env -> T.Text -> A.Value -> Env
maybeInsertIntoEnv env k v =
  case toEnvData v of
    Nothing -> env
    Just d -> H.insert k d env

-- | Convert an Aeson Object to an Env.
aesonToEnv :: A.Object -> Env
aesonToEnv = H.foldlWithKey' maybeInsertIntoEnv H.empty


data Resource
  = Single Page
  | Passthrough FilePath FilePath
  -- ^ in and out file paths

renderResource :: Resource -> PencilApp ()
renderResource (Single page) = render page
renderResource (Passthrough fpIn fpOut) = copyFile fpIn fpOut

renderResources :: [Resource] -> PencilApp ()
renderResources resources = forM_ resources renderResource

-- | Add the given Page into the given structure.
structurePage :: NonEmpty Page -> Page -> NonEmpty Page
structurePage structure page = NE.cons page structure

copyFile :: FilePath -> FilePath -> PencilApp ()
copyFile fpIn fpOut = do
  sitePrefix <- asks cSitePrefix
  outPrefix <- asks cOutPrefix
  liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory (outPrefix ++ fpOut))
  liftIO $ D.copyFile (sitePrefix ++ fpIn) (outPrefix ++ fpOut)

loadResourceAsHtml :: FilePath -> PencilApp Resource
loadResourceAsHtml = loadResourceWithFileModifier (\fp -> FP.dropExtension fp ++ ".html")

loadResourceId :: FilePath -> PencilApp Resource
loadResourceId = loadResourceWithFileModifier id

loadResourceWithFileModifier :: (FilePath -> FilePath) -> FilePath -> PencilApp Resource
loadResourceWithFileModifier fpf fp =
  -- If we can load the Page as text file, convert to a Single. Otherwise if it
  -- wasn't a text file, then return a Passthroguh resource. This is where we
  -- finally handle the "checked" exception; that is, converting the Left error
  -- case (NotTextFile) into a Right case (Passthrough).
  liftM Single (load fpf fp)
    `catchError` handle
  -- 'handle' requires FlexibleContexts
  where handle e = case e of
                     NotTextFile _ -> return (Passthrough fp (fpf fp))
                     _ -> throwError e

render :: Page -> PencilApp ()
render (Page nodes _ fpOut) = do
  outPrefix <- asks cOutPrefix
  let noFileName = FP.takeBaseName fpOut == ""
  let fpOut' = outPrefix ++ if noFileName then fpOut ++ "index.html" else fpOut
  liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory fpOut')
  liftIO $ TIO.writeFile fpOut' (renderNodes nodes)

-- | Apply and render the structure.
applyRender :: Env -> NonEmpty Page -> PencilApp ()
applyRender env structure = apply env structure >>= render

-- | Load page, extracting the tags and preamble variables. Renders Markdown
-- files into HTML. Defaults the page output file path to the given input file
-- path, but as a folder, with an HTML extension.
--
-- As an example, if the given fp is "/foo/bar/hello.markdown", the returned
-- filepath is "/foo/bar/hello.html".
loadHtml :: FilePath -> PencilApp Page
loadHtml = load (\fp -> FP.dropExtension fp ++ ".html")

loadId :: FilePath -> PencilApp Page
loadId = load id

load :: (FilePath -> FilePath) -> FilePath -> PencilApp Page
load fpf fp = do
  (_, nodes) <- parseTextFile fp
  let env = findEnv nodes
  let fp' = "/" ++ fpf fp
  let env' = H.insert "this.url" (EText (T.pack fp')) env
  return $ Page nodes env' ("/" ++ fpf fp)

-- | Find preamble node, and load as an Env. If no preamble is found, return a
-- blank Env.
findEnv :: [PNode] -> Env
findEnv nodes =
  aesonToEnv $ M.fromMaybe H.empty (findPreambleText nodes >>= (A.decode . encodeUtf8 . T.strip))

findPreambleText :: [PNode] -> Maybe T.Text
findPreambleText nodes = L.find isPreamble nodes >>= preambleText

isPreamble :: PNode -> Bool
isPreamble (PPreamble _) = True
isPreamble _ = False

preambleText :: PNode -> Maybe T.Text
preambleText (PPreamble t) = Just t
preambleText _ = Nothing

-- | Copy specified file from site to out.
renderCss :: FilePath -> PencilApp ()
renderCss fp =
  -- Drop .scss/sass extension and replace with .css.
  load (\f -> FP.dropExtension f ++ ".css") fp >>= render

