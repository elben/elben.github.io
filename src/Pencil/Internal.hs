{-# LANGUAGE OverloadedStrings, FlexibleContexts, FlexibleInstances #-}

module Pencil.Internal where

import Pencil.Internal.Env
import Pencil.Internal.Parser

import Control.Exception (tryJust)
import Control.Monad (forM_, foldM, filterM, liftM)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Char (toLower)
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Typeable (Typeable)
import GHC.IO.Exception (IOException(ioe_description, ioe_filename, ioe_type), IOErrorType(NoSuchThing))
import Text.EditDistance (levenshteinDistance, defaultEditCosts)
import Text.Sass.Options (defaultSassOptions)
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

-- | The main Config needed to build your website. See Pencil.Internal
-- for the full version. Use 'defaultConfig' as a starting point, along with the
-- config-modification helpers such as 'setSourceDir'.
data Config = Config
  { configSourceDir :: FilePath
  , configOutputDir :: FilePath
  , configEnv :: Env
  , configSassOptions :: Sass.SassOptions
  , configMarkdownOptions :: P.WriterOptions
  }

-- | This default Config gives you everything you need to start.
--
-- Default values:
--
-- @
-- Config
--  { 'configSourceDir' = "site/"
--  , 'configOutputDir' = "out/"
--  , 'configEnv' = HashMap.empty
--  , 'configSassOptions' = Text.Sass.Options.defaultSassOptions
--  , 'configMarkdownOptions' = Text.Pandoc.def { Text.Pandoc.writerHighlight = True }
--  }
-- @
--
defaultConfig :: Config
defaultConfig = Config
  { configSourceDir = "site/"
  , configOutputDir = "out/"
  , configEnv = H.empty
  , configSassOptions = Text.Sass.Options.defaultSassOptions
  , configMarkdownOptions = P.def { P.writerHighlight = True }
  }

getSourceDir :: Config -> FilePath
getSourceDir = configSourceDir

-- | Sets the source directory (where all your source files live).
setSourceDir :: FilePath -> Config -> Config
setSourceDir fp c = c { configSourceDir = fp }

getOutputDir :: Config -> FilePath
getOutputDir = configOutputDir

-- | Sets the output directory (where all your rendered files go).
setOutputDir :: FilePath -> Config -> Config
setOutputDir fp c = c { configOutputDir = fp }

getEnv :: Config -> Env
getEnv = configEnv

setEnv :: Env -> Config -> Config
setEnv env c = c { configEnv = env }

getSassOptions :: Config -> Sass.SassOptions
getSassOptions = configSassOptions

setSassOptions :: Sass.SassOptions -> Config -> Config
setSassOptions env c = c { configSassOptions = env }

getMarkdownOptions :: Config -> P.WriterOptions
getMarkdownOptions = configMarkdownOptions

setMarkdownOptions :: P.WriterOptions -> Config -> Config
setMarkdownOptions wo c = c { configMarkdownOptions = wo }

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
  sitePrefix <- asks getSourceDir
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

-- | A 'H.HashMap' of file extensions (e.g. @markdown@) to 'FileType'.
extensionMap :: H.HashMap String FileType
extensionMap = H.fromList
  [ ("markdown", Markdown)
  , ("md", Markdown)
  , ("html", Html)
  , ("htm", Html)
  , ("sass", Sass)
  , ("scss", Sass)]

-- | Takes a file path and returns the 'FileType', defaulting to 'Other' if it's
-- not a supported extension.
toExtension :: FilePath -> FileType
toExtension fp =
  -- takeExtension returns ".markdown", so drop the "."
  M.fromMaybe Other (H.lookup (map toLower (drop 1 (FP.takeExtension fp))) extensionMap)

-- | The Page is a fundamental data structure in Pencil. It contains the parsed
-- template of a file (e.g. of Markdown or HTML files). It may have template
-- directives (e.g. @${body}@) that has not yet been rendered, and an
-- environment loaded from the preamble section of the file. A Page also
-- contains 'pageFilePath', which is the output file path.
data Page = Page
  { pageNodes     :: [PNode]
  , pageEnv       :: Env
  , pageFilePath  :: FilePath
  -- ^ The rendered output path of this page. Defaults to the input file path.
  -- This file path is used to generate the self URL that is injected into the
  -- environment.
  } deriving (Eq, Show)

-- | Returns the 'Env' from a 'Page'.
getPageEnv :: Page -> Env
getPageEnv = pageEnv

-- | Sets the 'Env' in a 'Page'.
setPageEnv :: Env -> Page -> Page
setPageEnv env p = p { pageEnv = env }

-- | Applies the environment variables on the given pages.
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
-- > +--------------+
-- > |              | <--- default.html
-- > |              |      Defines ${websiteTitle}
-- > |  +---------+ |
-- > |  |         |<+----- blog-post.html
-- > |  | +-----+ | |      Renders ${postTitle}, ${postDate}
-- > |  | |     | | |
-- > |  | |     | | |
-- > |  | |     |<+-+----- blog-article-content.markdown
-- > |  | |     | | |      Renders ${websiteTitle}
-- > |  | +-----+ | |      Defines ${postTitle}, ${postDate}
-- > |  +---------+ |
-- > +--------------+
--
-- In this case, we want to accumulate the environment variables, starting from
-- default.html, to blog-post.html, and the markdown file's variables. Combine
-- all of that, then render the blog post content. This content is then injected
-- into the parent's environment as a $body variable, for use in blog-post.html.
-- Now *that* content is injected into the parent environment's $body variable,
-- which is then used to render the full-blown HTML page.
--
apply :: Structure -> PencilApp Page
apply pages = apply_ (NE.reverse pages)

-- It's simpler to implement if NonEmpty is ordered outer-structure first (e.g.
-- HTML layout).
apply_ :: Structure -> PencilApp Page
apply_ (Page nodes penv fp :| []) = do
  env <- asks getEnv
  let env' = H.union penv env -- LHS overrides RHS
  nodes' <- evalNodes env' nodes
  return $ Page nodes' env' fp
apply_ (Page nodes penv _ :| (headp : rest)) = do
  -- Modify the current env (in the ReaderT) with the one in the page (penv)
  Page nodes' env' fpInner <- local (\c -> setEnv (H.union penv (getEnv c)) c)
                                    (apply_ (headp :| rest))
  let env'' = H.insert "body" (EText (renderNodes nodes')) env'
  nodes'' <- evalNodes env'' nodes
  -- Get the inner-most Page's file path, and pass that upwards to the returned
  -- Page.
  return $ Page nodes'' env'' fpInner

-- | Loads the given file as a text file. Throws an exception into the ExceptT
-- monad transformer if the file is not a text file.
loadTextFile :: FilePath -> PencilApp T.Text
loadTextFile fp = do
  sitePrefix <- asks getSourceDir
  -- Try to read the file. If it fails because it's not a text file, capture the
  -- exception and convert it to a "checked" exception in the ExceptT stack via
  -- 'throwError'.
  eitherContent <- liftIO $ tryJust toPencilException (TIO.readFile (sitePrefix ++ fp))
  case eitherContent of
    Left e -> throwError e
    Right a -> return a

-- | Converts the IOError to a known 'PencilException'.
--
-- How to test errors:
--
-- @
-- import Control.Exception
-- import qualified Data.Text.IO as TIO
--
-- (\e -> print (ioe_description (e :: IOError)) >> return "") `handle` (TIO.readFile "foo")
-- @
--
toPencilException :: IOError -> Maybe PencilException
toPencilException e
  | isInvalidByteSequence e = Just (NotTextFile e)
  | isNoSuchFile e = Just (FileNotFound (ioe_filename e))
  | otherwise = Nothing

-- | Returns true if the IOError is an invalid byte sequence error. This
-- suggests that the file is a binary file.
isInvalidByteSequence :: IOError -> Bool
isInvalidByteSequence e = ioe_description e == "invalid byte sequence"

-- | Returns true if the IOError is due to missing file.
isNoSuchFile :: IOError -> Bool
isNoSuchFile e = ioe_type e == NoSuchThing

-- | Loads and parses the given file path. Converts 'Markdown' files to HTML,
-- compiles 'Sass' files into CSS, and leaves everything else alone.
parseAndConvertTextFiles :: FilePath -> PencilApp (T.Text, [PNode])
parseAndConvertTextFiles fp = do
  content <- loadTextFile fp
  content' <-
    case toExtension fp of
      Markdown -> do
        markdownOptions <- asks getMarkdownOptions
        case P.readMarkdown P.def (T.unpack content) of
          Left _ -> return content
          Right pandoc -> return $ T.pack $ P.writeHtmlString markdownOptions pandoc
      Sass -> do
        sassOptions <- asks getSassOptions
        sitePrefix <- asks getSourceDir
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
-- because of @${partial(..)}@ calls that requires us to load a file.
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
  (_, nodes) <- parseAndConvertTextFiles (T.unpack fp)
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
groupByElements :: T.Text
                -> [Page]
                -> H.HashMap T.Text [Page]
groupByElements var pages =
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
    -- Reverse to keep ordering consistent inside hash map, since the fold
    -- prepends into accumulated list.
    (reverse pages)

loadResourcesId :: Bool -> Bool -> FilePath -> PencilApp [Resource]
loadResourcesId recursive strict = loadResources recursive strict id

-- | Load directory as Resources.
loadResources :: Bool -> Bool -> (FilePath -> FilePath) -> FilePath -> PencilApp [Resource]
loadResources recursive strict fpf dir = do
  fps <- listDir recursive dir
  if strict
    then return $ map (\fp -> Passthrough fp fp) fps
    else mapM (loadResource fpf) fps

-- List files in directory, optionally recursively. Returns paths that include
-- the given dir.
listDir :: Bool -> FilePath -> PencilApp [FilePath]
listDir recursive dir = do
  let dir' = FP.addTrailingPathSeparator dir
  fps <- listDir_ recursive dir'
  return $ map (dir' ++) fps

listDir_ :: Bool -> FilePath -> PencilApp [FilePath]
listDir_ recursive dir = do
  sitePrefix <- asks getSourceDir
  -- List files (just the filename, without the fp directory prefix)
  listing <- liftIO $ D.listDirectory (sitePrefix ++ dir)
  -- Filter only for files (we have to add the right directory prefixes to the
  -- file check)
  files <- liftIO $ filterM (\f -> D.doesFileExist (sitePrefix ++ dir ++ f)) listing
  dirs <- liftIO $ filterM (\f -> D.doesDirectoryExist (sitePrefix ++ dir ++ f)) listing

  innerFiles <- if recursive
                  then mapM
                         (\d -> do
                           ff <- listDir_ recursive (dir ++ d ++ "/")
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

copyFile :: FilePath -> FilePath -> PencilApp ()
copyFile fpIn fpOut = do
  sitePrefix <- asks getSourceDir
  outPrefix <- asks getOutputDir
  liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory (outPrefix ++ fpOut))
  liftIO $ D.copyFile (sitePrefix ++ fpIn) (outPrefix ++ fpOut)

asHtml :: FilePath -> FilePath
asHtml fp = FP.dropExtension fp ++ ".html"

-- | Convert a file path into a directory name, dropping the extension.
-- Pages with a directory as its FilePath is rendered as an index file in that
-- directory. For example, the @pages/about.html@ is transformed into
-- @pages/about/@, which 'render' would turn the 'Page' into
-- @pages/about/index.html'.
--
asDir :: FilePath -> FilePath
asDir fp = FP.replaceFileName fp (FP.takeBaseName fp) ++ "/"

asCss :: FilePath -> FilePath
asCss fp = FP.dropExtension fp ++ ".css"

-- | A file modifier that renames to HTML files that will be rendered to HTML.
markdownAsHtml :: FilePath -> FilePath
markdownAsHtml fp =
  case toExtension fp of
    Markdown -> FP.dropExtension fp ++ ".html"
    _ -> fp

loadResourceId :: FilePath -> PencilApp Resource
loadResourceId = loadResource id

loadResource :: (FilePath -> FilePath) -> FilePath -> PencilApp Resource
loadResource fpf fp =
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

loadId :: FilePath -> PencilApp Page
loadId = load id

-- | Loads a file into a Page, rendering the file (as determined by the file
-- extension) into the proper output format (e.g. Markdown rendered to
-- HTML, SCSS to CSS). Parses the template directives and preamble variables
-- into its environment. The 'Page''s 'pageFilePath' is determined by the given
-- function, which expects the original file path, and returns the designated file
-- path.
--
-- The Page's designated file path is calculated and stored in the Page's
-- environment in the variable @this.url@. This allows the template to use
-- @${this.url}@ to refer to the designated file path.
--
-- Example:
--
-- @
-- -- Loads index.markdown with the designated file path of index.html.
-- load 'asHtml' "index.markdown"
-- @
--
load :: (FilePath -> FilePath) -> FilePath -> PencilApp Page
load fpf fp = do
  (_, nodes) <- parseAndConvertTextFiles fp
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
  load asCss fp >>= render

type Structure = NonEmpty Page

-- | Create a new structure from two Pages.
(<||) :: Page -> Page -> Structure
(<||) x y = y :| [x]

-- | Push Page into Structure.
(<|) :: Structure -> Page -> Structure
(<|) ne x = NE.cons x ne

-- | Convert a Page into a Structure.
structure :: Page -> Structure
structure p = p :| []

withEnv :: Env -> PencilApp a -> PencilApp a
withEnv env = local (setEnv env)

class Render a where
  render :: a -> PencilApp ()

instance Render Resource where
  render (Single page) = render page
  render (Passthrough fpIn fpOut) = copyFile fpIn fpOut

-- This requires FlexibleInstances.
instance Render [Resource] where
  render resources = forM_ resources render

-- This requires FlexibleInstances.
instance Render Structure where
  render s = apply s >>= render

instance Render Page where
  render (Page nodes _ fpOut) = do
    outPrefix <- asks getOutputDir
    let noFileName = FP.takeBaseName fpOut == ""
    let fpOut' = outPrefix ++ if noFileName then fpOut ++ "index.html" else fpOut
    liftIO $ D.createDirectoryIfMissing True (FP.takeDirectory fpOut')
    liftIO $ TIO.writeFile fpOut' (renderNodes nodes)

