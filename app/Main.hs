{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil.Parser
import Pencil.Env
import Control.Monad (forM_, foldM)
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import qualified CMark as CM
import qualified Data.HashMap.Strict as H
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.HTML.TagSoup as TS
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import qualified Data.List.NonEmpty as NE

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
-- "/posts/2011-01-01-the-post-title.html" => "/posts/the-post-title/"
blogPostUrl :: FilePath -> FilePath
blogPostUrl fp = FP.replaceFileName fp (drop 11 (FP.takeBaseName fp)) ++ "/"

-- | Modify a variable in the env.
modifyEnvVar :: Page -> (EnvData -> EnvData) -> T.Text -> Page
modifyEnvVar (Page nodes env fp) f k =
  let env' = H.adjust f k env
  in Page nodes env' fp

renderBlogPost :: NonEmpty Page -> FilePath -> IO ()
renderBlogPost structure fp = do
  page <- loadPageWithFileModifier blogPostUrl fp
  applyPage globalEnv (NE.cons page structure) >>= renderPage

loadAndApplyPage :: NonEmpty Page -> FilePath -> IO ()
loadAndApplyPage structure fp = do
  page <- loadPage fp
  applyPage globalEnv (NE.cons page structure) >>= renderPage

main :: IO ()
main = do
  pageLayout <- loadPage "layouts/default.html"
  pagePartial <- loadPage "partials/post.html"

  -- Load posts
  posts <- mapM
    (loadPageWithFileModifier blogPostUrl)
    [ "posts/2010-01-30-behind-pythons-unittest-main.markdown"
    , "posts/2010-04-16-singleton-pattern-in-python.markdown"
    ]

  forM_
    [ "posts/2010-01-30-behind-pythons-unittest-main.markdown"
    , "posts/2010-04-16-singleton-pattern-in-python.markdown"
    ]
    (renderBlogPost (pagePartial :| [pageLayout]))

  -- Index
  let postsEnv = H.insert "posts" (EList (map getPageEnv posts)) globalEnv
  indexPage <- loadPage "index.html"
  applyPage postsEnv (indexPage :| [pageLayout]) >>= renderPage

  -- Write CSS file
  includeAsset "stylesheets/default.css"

parseMaybeText :: T.Text -> A.Object -> Maybe T.Text
parseMaybeText k = parseMaybe (\o -> o A..: k :: Parser T.Text)

-- | Convert known Aeson types into known Env types.
-- TODO: support array of env vars
maybeInsertIntoEnv :: Env -> T.Text -> A.Value -> Env
maybeInsertIntoEnv env k (A.String s) =
  case toDateTime (T.unpack s) of
    Nothing -> H.insert k (EText s) env
    Just dt -> H.insert k (EDateTime dt) env
maybeInsertIntoEnv env _ _ = env

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
  } deriving (Eq, Show)

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
loadPage :: FilePath
         -> IO Page
loadPage = loadPageWithFileModifier (\fp -> FP.dropExtension fp ++ ".html")

loadPageWithFileModifier :: (FilePath -> FilePath) -> FilePath -> IO Page
loadPageWithFileModifier fpf fp = do
  (content, nodes) <- parsePage fp
  let env = aesonToEnv $ loadVariables (TS.parseTags content)
  let fp' = "/" ++ fpf fp
  let env' = H.insert "this.url" (EText (T.pack fp')) env
  return $ Page nodes env' ("/" ++ fpf fp)

parsePage :: FilePath -> IO (T.Text, [PNode])
parsePage fp = do
  content <- TIO.readFile (sitePrefix ++ fp)
  let extension = FP.takeExtension fp
  let content' =
        if extension `elem` [".markdown", ".md"]
        then CM.commonmarkToHtml [] content
        else content
  let nodes = case runParser content' of
                Left _ -> []
                Right n -> n
  return (content', nodes)

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
    Just (EList envs) -> do
      -- Render the for nodes once for each given env, and append them together
      forNodes <-
        foldM
          (\accNodes e -> do
              nodes' <- evalNodes (H.union e env) nodes
              return $ accNodes ++ nodes')
          [] envs
      return $ forNodes ++ rest'
    -- Var is not an EList; everything inside the for-statement is thrown away
    Just _ -> return rest'
evalNodes env (PPartial fp : rest) = do
  (_, nodes) <- parsePage (T.unpack fp)
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
includeAsset :: FilePath -> IO ()
includeAsset fp = do
  -- True flag is to create parents too
  D.createDirectoryIfMissing True (outPrefix ++ FP.takeDirectory fp)
  D.copyFile (sitePrefix ++ fp) (outPrefix ++ fp)

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

