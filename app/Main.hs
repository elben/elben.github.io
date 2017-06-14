{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil.Parser
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import qualified Text.ParserCombinators.Parsec as P
import qualified CMark as CM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import qualified Text.HTML.TagSoup as TS
import qualified Data.HashMap.Strict as H
import qualified Data.List as DL
import qualified Data.Maybe as M
import Debug.Trace

sitePrefix :: String
sitePrefix = "site/"

outPrefix :: String
outPrefix = "out/"

main :: IO ()
main = do
  layoutText <- TIO.readFile "site/layouts/default.html"
  text <- TIO.readFile "site/index.html"
  let tags = TS.parseTags layoutText
  let newTags = injectIntoHead (cssTag "stylesheets/default.css") (injectIntoBodyVar (TS.parseTags text) tags)
  let rendered = TS.renderTags newTags
  TIO.writeFile "out/index.html" rendered

  -- Write CSS file
  includeAsset "stylesheets/mysheet.css"
  includeAsset "stylesheets/default.css"

  -- Load posts
  -- TODO load post HTML into partial, into template.
  (tags1, vars1) <- loadPage "site/posts/2010-01-30-behind-pythons-unittest-main.markdown"
  (tags2, vars2) <- loadPage "site/posts/2010-04-16-singleton-pattern-in-python.markdown"
  print vars1
  print vars2
  -- Wrong place to apply variables. Need to make sure the content is injected
  -- into the higher-level partials first.
  let replacedTags1 = replaceVarsInTags vars1 tags1
  -- let replacedTags2 = replaceVarsInTags vars2 tags2
  let blogPostHtml1 = CM.commonmarkToHtml [] (TS.renderTags replacedTags1)
  -- let blogPostHtml2 = CM.commonmarkToHtml [] (TS.renderTags replacedTags2)

  -- Try to add the first blog post into the partial
  partialText <- TIO.readFile "site/partials/post.html"
  (partialTags, partialVars) <- loadPage "site/partials/post.html"

  -- Insert an Aeson string as the "body" variable.
  let completeVar1 = H.union partialVars (H.insert "body" (EHtml replacedTags1) vars1)

  print completeVar1
  print partialText

  let coolHtml = replaceVarsInText completeVar1 partialText
  TIO.writeFile "out/posts/2010-01-30-behind-pythons-unittest-main.html" coolHtml
  -- TIO.writeFile "out/posts/2010-04-16-singleton-pattern-in-python.html" blogPostHtml2

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

maybeInsertIntoEnv :: Env -> T.Text -> Value -> Env
maybeInsertIntoEnv env k (String s) = H.insert k (EText s) env
maybeInsertIntoEnv env k unknown = env

aesonToEnv :: Object -> Env
aesonToEnv o =
  let keys = H.keys o
  in H.foldlWithKey'
      maybeInsertIntoEnv
      H.empty o

loadAndApplyPage :: FilePath -> IO T.Text
loadAndApplyPage fp = do
  (tags, value) <- loadPage fp
  let tags' = replaceVarsInTags value tags
  return $ TS.renderTags tags'

loadPage :: FilePath -> IO (Tags, Env)
loadPage fp = do
  content <- TIO.readFile fp
  let content' =
        if ".markdown" `DL.isSuffixOf` fp
        then CM.commonmarkToHtml [] content
        else content
  let tags = TS.parseTags content'
  return (tags, aesonToEnv $ loadVariables tags)

-- Given a Env object, search through tags and replace any occurence of
-- variables with the value in the JSON object, if present. If the variable is
-- missing, just skip it for now.
replaceVarsInTags :: Env -> Tags -> Tags
replaceVarsInTags _ [] = []
replaceVarsInTags env (TS.TagText str : rest) =
  TS.TagText (replaceVarsInText env str) : replaceVarsInTags env rest
replaceVarsInTags env (t : rest) =
  t : replaceVarsInTags env rest

-- "I have ${one} and ${two} variables and ${three}.
replaceVarsInText :: Env -> T.Text -> T.Text
replaceVarsInText env text =
  case P.parse parseEverything (T.unpack "") (T.unpack text) of
    Left _ -> (traceShow "I have gone left!!!" text)
    Right exprs ->
      joinExprs $ replaceVarsInExpression env (traceShowId exprs)

joinExprs :: [SimpleTemplate] -> T.Text
joinExprs = joinExprs' ""

joinExprs' :: T.Text -> [SimpleTemplate] -> T.Text
joinExprs' curr [] = curr
joinExprs' curr (STText text : rest) = joinExprs' (T.append curr text) rest
joinExprs' curr (STVar varName : rest) = joinExprs' (T.append curr varName) rest

replaceVarsInExpression :: Env -> [SimpleTemplate] -> [SimpleTemplate]
replaceVarsInExpression _ [] = []
replaceVarsInExpression env (STVar varName : rest) =
  case H.lookup varName env of
    Nothing -> STVar varName : rest
    Just envData ->
      -- TODO envDataToDisplay may spit out HTML that we need to not escape. We
      -- need to return Tags??
      STText (envDataToDisplay envData) : replaceVarsInExpression env rest
replaceVarsInExpression env (expr : rest) =
  expr : replaceVarsInExpression env rest

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

includeAsset :: FilePath -> IO ()
includeAsset f = D.copyFile (sitePrefix ++ f) (outPrefix ++ f)

-- Type alias
type Tags = [TS.Tag T.Text]

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

injectIntoHead :: Tags -> Tags -> Tags
injectIntoHead = injectInto "head"

injectInto :: T.Text -> Tags -> Tags -> Tags
injectInto _ _ [] = []
injectInto tagName inject (tagOpen @ (TS.TagOpen tag _) : rest) =
  if tag == tagName
  then tagOpen : inject ++ rest
  else tagOpen : injectInto tagName inject rest
injectInto tagName inject (tag : rest) =
  tag : injectInto tagName inject rest

