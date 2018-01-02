{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil.Parser
import Pencil.Env
import Control.Monad (forM_)
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import qualified CMark as CM
import qualified Data.HashMap.Strict as H
import qualified Data.List as DL
import qualified Data.Maybe as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import qualified System.FilePath as FP
import qualified Text.HTML.TagSoup as TS
import Debug.Trace

type PTags = [PTag]

sitePrefix :: String
sitePrefix = "site/"

outPrefix :: String
outPrefix = "out/"

globalEnv :: Env
globalEnv = H.fromList [("title", EText "Elben Shira's Awesome Website")]

-- TODO we need a data structure (use PageTarget?) where you can say: "please
-- load this blog post, and render it to this partial, and render it to this
-- template". It's a tree/linked-list thing, starting for the partial as the
-- top-most, down to the content
data Layout = LayoutNode Page Layout
            | LayoutLeaf Page

-- | Apply the given layout with the layout's enviornment merged with the given
-- previous environment.
--
-- If the parent layout is a node and has other (children) layouts inside of it,
-- it will first apply the childern layouts, applying the parent layout's
-- environment for the childern's environment. Once all the environments are
-- merged downwards into the stack, the lowest layout then renders its template
-- variables and returns the rendered page. The parent layout then takes that
-- rendered page and injects it into the "body" variable in its new environment,
-- which will also include variables declared in the childern layouts.
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
-- TODO can convert Layout data structrue to just a [Page]
applyLayout :: Env -> Layout -> Page
applyLayout env (LayoutLeaf (Page nodes penv fp)) =
  let env' = H.union penv env -- LHS overrides RHS
      nodes' = replaceVarsInTemplate env' nodes
  in Page nodes' env' fp
applyLayout env (LayoutNode (Page nodes penv fp) layout) =
  let Page nodes' env' _ = applyLayout (H.union penv env) layout
      env'' = H.insert "body" (EText (renderNodes nodes')) env'
      nodes'' = replaceVarsInTemplate env'' nodes
   in Page nodes'' env'' fp

renderBlogPost :: FilePath -> IO ()
renderBlogPost fp = do
  pageLayout <- loadPage "layouts/default.html"
  pagePartial <- loadPage "partials/post.html"
  page <- loadPage fp

  let layout = LayoutNode pageLayout (LayoutNode pagePartial (LayoutLeaf page))
  let Page nodes _ _ = applyLayout globalEnv layout

  -- "/posts/2011-01-01-the-post-title" => "/posts/the-post-title/"
  let fp' = FP.replaceFileName (getPageFilePath page) (drop 11 (FP.takeFileName (getPageFilePath page))) ++ "/"
  renderPage $ PageTarget nodes fp'

doPage :: Env -> Page -> IO ()
doPage env (Page nodes penv fp) = do
  Page layoutNodes layoutEnv _ <- loadPage "layouts/default.html"
  -- layoutText <- TIO.readFile "site/layouts/default.html"
  -- let layoutTags = injectIntoHead (cssTag "/stylesheets/default.css") (TS.parseTags layoutText)

  let env' = H.union env penv
  let nodes' = replaceVarsInTemplate env' nodes
  Page partialNodes partialEnv _ <- loadPage "partials/post.html"

  -- Insert an Aeson string as the "body" variable. Prefer LHS keys for dupes.
  let env'' = H.union (H.insert "body" (EText (renderNodes nodes')) env') partialEnv

  let partialNodes' = replaceVarsInTemplate env'' partialNodes

  let env''' = H.insert "body" (EHtml (TS.parseTags (renderNodes partialNodes'))) env''
  let layoutNodes' = replaceVarsInTemplate env''' layoutNodes

  -- "/posts/2011-01-01-the-post-title" => "/posts/the-post-title/"
  let fp' = FP.replaceFileName fp (drop 11 (FP.takeFileName fp)) ++ "/"
  renderPage $ PageTarget layoutNodes' fp'

main :: IO ()
main = do
  -- Layout
  layoutText <- TIO.readFile "site/layouts/default.html"
  let layoutTags = injectIntoHead (cssTag "/stylesheets/default.css") (TS.parseTags layoutText)

  -- Load posts
  -- posts <- mapM loadPage
  --                 [ "posts/2010-01-30-behind-pythons-unittest-main.markdown"
  --                 , "posts/2010-04-16-singleton-pattern-in-python.markdown"
  --                 ]
  forM_
    [ "posts/2010-01-30-behind-pythons-unittest-main.markdown"
    , "posts/2010-04-16-singleton-pattern-in-python.markdown"
    ]
    renderBlogPost
  -- Index
  Page indexNodes indexEnv _ <- loadPage "index.html"
  let indexTags = injectIntoBodyVar (TS.parseTags (renderNodes indexNodes)) layoutTags
  let indexNodesReplaced = replaceVarsInText (H.union globalEnv indexEnv) (TS.renderTags indexTags)

  TIO.writeFile "out/index.html" indexNodesReplaced

  -- Write CSS file
  includeAsset "stylesheets/mysheet.css"
  includeAsset "stylesheets/default.css"

  -- Render posts
  -- forM_ posts (doPage globalEnv)

parseMaybeText :: T.Text -> Object -> Maybe T.Text
parseMaybeText k = parseMaybe (\o -> o .: k :: Parser T.Text)

-- | Convert known Aeson types into known Env types.
-- TODO: support array of env vars
maybeInsertIntoEnv :: Env -> T.Text -> Value -> Env
maybeInsertIntoEnv env k (String s) = H.insert k (EText s) env
maybeInsertIntoEnv env _ _ = env

-- | Convert an Aeson Object to an Env.
aesonToEnv :: Object -> Env
aesonToEnv = H.foldlWithKey' maybeInsertIntoEnv H.empty

-- Describes a loaded page, with the page's template nodes, loaded environment
-- from the preamble, and where the page was loaded from.
data Page = Page
  { getPageNodes     :: [PNode]
  , getPageEnv       :: Env
  , getPageFilePath  :: String
  -- ^ The original filename, without extension and site prefix
  } deriving (Eq, Show)

-- Describes a target a page is to be rendered to. If the FilePath is a
-- directory, the page renderer will automatically target index.html inside that
-- directory.
data PageTarget = PageTarget [PNode] FilePath

renderPage :: PageTarget -> IO ()
renderPage (PageTarget nodes fpOut) = do
  let noFileName = FP.takeBaseName fpOut == ""
  let fpOut' = outPrefix ++ if noFileName then fpOut ++ "index.html" else fpOut
  D.createDirectoryIfMissing True (FP.takeDirectory fpOut')
  TIO.writeFile fpOut' (renderNodes nodes)
  return ()

-- | Load page, extracting the tags and preamble variables. Renders Markdown
-- files into HTML.
loadPage :: FilePath
         -> IO Page
loadPage fp = do
  -- foo/bar/file.markdown -> foo/bar/file
  content <- TIO.readFile (sitePrefix ++ fp)
  let fp' = FP.dropExtension fp
  let extension = FP.takeExtension fp
  let env = aesonToEnv $ loadVariables (TS.parseTags content)
  let content' =
        if extension `elem` [".markdown", ".md"]
        then CM.commonmarkToHtml [] content
        else content
  let nodes = case runParser content' of
                Left _ -> []
                Right nodes -> nodes
  return $ Page nodes env fp'

replaceVarsInTemplate :: Env -> [PNode] -> [PNode]
replaceVarsInTemplate _ [] = []
replaceVarsInTemplate env (PVar var : rest) =
  case H.lookup var env of
    Nothing -> PVar var : replaceVarsInTemplate env rest
    Just envData -> PText (envDataToDisplay envData) : replaceVarsInTemplate env rest
replaceVarsInTemplate env (n : rest) = n : replaceVarsInTemplate env rest

replaceVarsInText :: Env -> T.Text -> T.Text
replaceVarsInText env text =
  case runParser text of
    Left _ -> traceShow ("I have gone left!!!" :: String) text
    Right nodes -> renderNodes $ replaceVarsInTemplate env nodes

-- Find the PREAMBLE JSON section, parse it, and return as an Aeson Object.
loadVariables :: Tags -> Object
loadVariables tags =
  case findPreambleComment tags of
    Nothing -> H.empty
    Just commentText ->
      let v = decode (fromStrict $ encodeUtf8 (T.strip commentText)) :: Maybe Object
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
-- TODO use the Parser we wrote instead of this custom thing
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

