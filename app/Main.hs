{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import qualified CMark as CM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import qualified Text.HTML.TagSoup as TS
import qualified Data.HashMap.Strict as H
import qualified Data.Maybe as M
import Debug.Trace

type Tags = [TS.Tag T.Text]
type PTags = [PTag]

sitePrefix :: String
sitePrefix = "site2/"

outPrefix :: String
outPrefix = "out/"

main :: IO ()
main = do
  layoutText <- TIO.readFile "site2/layouts/default.html"
  text <- TIO.readFile "site2/index.html"
  let layoutTags = TS.parseTags layoutText
  let indexTags = injectIntoHead (cssTag "stylesheets/default.css") (injectIntoBodyVar (TS.parseTags text) layoutTags)
  let indexHtml = TS.renderTags indexTags
  TIO.writeFile "out/index.html" indexHtml

  -- Write CSS file
  includeAsset "stylesheets/mysheet.css"
  includeAsset "stylesheets/default.css"

  -- Load posts
  (text1, vars1) <- loadAndApplyPage "site2/posts/2010-01-30-behind-pythons-unittest-main.markdown"
  let html1 = CM.commonmarkToHtml [] text1

  -- Try to add the first blog post into the partial
  partialText <- TIO.readFile "site2/partials/post.html"
  (partialNodes, partialVars) <- loadPage "site2/partials/post.html"

  -- Insert an Aeson string as the "body" variable.
  let vars1' = H.union partialVars (H.insert "body" (EHtml (TS.parseTags html1)) vars1)

  print ("!========== 1" :: String)
  print vars1'
  print ("!========== 2" :: String)
  print partialNodes

  let partialTextReplaced = replaceVarsInText vars1' partialText
  print ("!========== 3" :: String)
  print partialTextReplaced

  let vars1'' = H.union partialVars (H.insert "body" (EHtml (TS.parseTags partialTextReplaced)) vars1')
  let layoutTextForPost1 = replaceVarsInText vars1'' layoutText

  TIO.writeFile "out/posts/2010-01-30-behind-pythons-unittest-main.html" layoutTextForPost1

-- Environment variables, for template variables.
-- We need to do this smartly, like a real PL. Example, say we want to make a
-- page that list all our blog posts. Well, each blog post has variables, all of
-- the same name (e.g. postTitle). So this is variable replacement/application
-- problem aka closures (kind of).
--
--
-- - Layout: the basic layout
--   - Variables used: title
--   - Variables declared: color-of-the-week
--   - Partial: list of blog posts
--       - Variable declared: title
--       - Stylesheet needed: blog-post-list.css
--       - Partial: blog post snippet
--         - Variables used: postTitle, date
--         - Blog post: How I win
--           - Variables declared: title, postTitle, date
--           - Variables used: color-of-the-week
--           - JavaScript needed: mathjax (optional to implement)
--           - Stylesheet needed: dark themed (optional to implement)
--       - Partial: blog post snippet
--         - Variables used: postTitle, date
--         - Blog post: Why I'm awesome
--           - Variables declared: postTitle

-- We should use that hack that allows ppl to extend this with their own types?
-- Example: we want a "tags" type for a list of blog post tags
-- https://two-wrongs.com/dynamic-dispatch-in-haskell-how-to-make-code-extendable
data EnvData =
  EText T.Text
  | EHtml Tags
  deriving (Eq, Show)

type Env = H.HashMap T.Text EnvData

envDataToDisplay :: EnvData -> T.Text
envDataToDisplay (EText t) = t
envDataToDisplay (EHtml tags) = TS.renderTags tags

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

loadAndApplyPage :: FilePath -> IO (T.Text, Env)
loadAndApplyPage fp = do
  (nodes, env) <- loadPage fp
  print ("*********** 1 *******" :: String)
  print nodes
  print ("*********** 2 *******" :: String)
  print env
  let nodes' = replaceVarsInTemplate env nodes
  print ("*********** 3 *******" :: String)
  print nodes'
  return (renderNodes nodes', env)

-- | Load page, extracting the tags and preamble variables.
loadPage :: FilePath -> IO ([PNode], Env)
loadPage fp = do
  content <- TIO.readFile fp
  let env = aesonToEnv $ loadVariables (TS.parseTags content)
  case runParser content of
    Left _ -> return ([], env)
    Right nodes -> return (nodes, env)

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
includeAsset f = D.copyFile (sitePrefix ++ f) (outPrefix ++ f)

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

