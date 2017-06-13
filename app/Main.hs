{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson
import Data.Aeson.Types (emptyObject)
import Data.ByteString.Lazy (fromStrict)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import qualified Text.ParserCombinators.Parsec as P
import qualified CMark as CM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import qualified Text.HTML.TagSoup as TS
import qualified Data.HashMap.Strict as H
import qualified Data.List as DL
import qualified Data.Maybe as M

sitePrefix :: String
sitePrefix = "site/"

outPrefix :: String
outPrefix = "out/"

main :: IO ()
main = do
  layoutText <- TIO.readFile "site/layouts/default.html"
  text <- TIO.readFile "site/index.html"
  putStr $ T.unpack text
  let tags = TS.parseTags layoutText
  putStr $ show tags
  let newTags = injectIntoHead (cssTag "stylesheets/default.css") (injectIntoBodyVar (TS.parseTags text) tags)
  putStr $ show newTags
  let rendered = TS.renderTags newTags
  putStr $ T.unpack rendered
  TIO.writeFile "out/index.html" rendered

  -- Write CSS file
  includeAsset "stylesheets/mysheet.css"
  includeAsset "stylesheets/default.css"

  -- Load posts
  (tags1, vars1) <- loadPage "site/posts/2010-01-30-behind-pythons-unittest-main.markdown"
  (tags2, vars2) <- loadPage "site/posts/2010-01-30-behind-pythons-unittest-main.markdown"
  print vars1
  print vars2
  let replacedTags1 = replaceVarsInTags vars1 tags1
  let replacedTags2 = replaceVarsInTags vars2 tags2
  print replacedTags1
  print replacedTags2
  let blogPostHtml1 = CM.commonmarkToHtml [] (TS.renderTags replacedTags1)
  let blogPostHtml2 = CM.commonmarkToHtml [] (TS.renderTags replacedTags2)
  TIO.writeFile "out/posts/2010-01-30-behind-pythons-unittest-main.html" blogPostHtml1
  TIO.writeFile "out/posts/2010-04-16-singleton-pattern-in-python.html" blogPostHtml2

loadAndApplyPage :: FilePath -> IO T.Text
loadAndApplyPage fp = do
  (tags, value) <- loadPage fp
  let tags' = replaceVarsInTags value tags
  return $ TS.renderTags tags'

loadPage :: FilePath -> IO (Tags, Object)
loadPage fp = do
  content <- TIO.readFile fp
  let content' =
        if ".markdown" `DL.isSuffixOf` fp
        then CM.commonmarkToHtml [] content
        else content
  let tags = TS.parseTags content'
  return (tags, loadVariables tags)

-- Given a JSON object, search through tags and replace any occurence of
-- variables with the value in the JSON object, if present. If the variable is
-- missing, just skip it for now.
replaceVarsInTags :: Object -> Tags -> Tags
replaceVarsInTags object [] = []
replaceVarsInTags object (TS.TagText str : rest) =
  TS.TagText (replaceVarsInText object str) : replaceVarsInTags object rest

data SimpleTemplate =
    STText T.Text
  | STVar T.Text
  deriving (Show, Eq)

-- "I have ${one} and ${two} variables and ${three}.
replaceVarsInText :: Object -> T.Text -> T.Text
replaceVarsInText object text =
  case P.parse parseEverything (T.unpack "") (T.unpack text) of
    Left err -> text
    Right exprs ->
      case replaceVarsInExpression object exprs of
        Nothing -> text
        Just evalExprs -> joinExprs evalExprs

joinExprs :: [SimpleTemplate] -> T.Text
joinExprs = joinExprs' ""

joinExprs' :: T.Text -> [SimpleTemplate] -> T.Text
joinExprs' curr [] = curr
joinExprs' curr (STText text : rest) = joinExprs' (T.append curr text) rest

replaceVarsInExpression :: Object -> [SimpleTemplate] -> [SimpleTemplate]
replaceVarsInExpression object [] = []
replaceVarsInExpression object (STVar varName : rest) =
  case H.lookup varName object of
    Nothing -> STVar varName : rest
    Just val -> STText "" : replaceVarsInExpression object rest
replaceVarsInExpression object (head : rest) =
  head : replaceVarsInExpression object rest

-- | Parse variables.
--
-- >>> parse parseVar "" "${ffwe}"
-- Right (STVar "ffwe")
--
-- >>> isLeft $ parse parseVar "" "Hello ${name}"
-- True
parseVar :: P.Parser SimpleTemplate
parseVar = do
  _ <- P.char '$'
  _ <- P.char '{'
  varName <- (P.many (P.noneOf "}"))
  _ <- P.char '}'
  return $ STVar (T.pack varName)

parseContent :: P.Parser SimpleTemplate
parseContent = do
  stuff <- P.many1 (P.noneOf "$")
  return $ STText (T.pack stuff)

-- | Parse everything.
--
-- >>> parse parseEverything "" "Hello ${name}, how are things going in ${city}?"
parseEverything :: P.Parser [SimpleTemplate]
parseEverything = P.many1 (parseContent P.<|> parseVar)

-- Should use parsec for this.
-- parseTextIntoSimpleTemplate' :: [SimpleTemplate] -> T.Text -> [SimpleTemplate]
-- parseTextIntoSimpleTemplate' current "" = current
-- -- parseTextIntoSimpleTemplate' current ("${" ++ text) = current
-- parseTextIntoSimpleTemplate' (STText currText : old) (c : text) =
--   parseTextIntoSimpleTemplate' ((STText (c : currText : old)) text

loadVariables :: Tags -> Object
loadVariables tags =
  case findPreambleComment tags of
    Nothing -> H.empty
    Just commentText ->
      let v = decode (fromStrict $ encodeUtf8 (T.strip commentText)) :: Maybe Object
       in M.fromMaybe H.empty v

findPreambleComment :: Tags -> Maybe T.Text
findPreambleComment [] = Nothing
findPreambleComment (tag @ (TS.TagComment str) : rest) =
  if T.isPrefixOf "PREAMBLE" (T.strip str)
     then let (a, b) = T.breakOn "PREAMBLE" str
           in Just $ T.replace "PREAMBLE" "" b
  else findPreambleComment rest
findPreambleComment (tag : rest) =
  findPreambleComment rest

includeAsset :: FilePath -> IO ()
includeAsset f = D.copyFile (sitePrefix ++ f) (outPrefix ++ f)

-- Type alias
type Tags = [TS.Tag T.Text]

cssTag :: T.Text -> Tags
cssTag file = TS.parseTags $ T.append "<link rel=\"stylesheet\" href=\"" $ T.append file "\" />"

-- Inject tags into the body tag, at the ${body} annotation location.
injectIntoBodyVar :: Tags -> Tags -> Tags
injectIntoBodyVar inject [] = []
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
injectInto tagName inject [] = []
injectInto tagName inject (tagOpen @ (TS.TagOpen tag attrs) : rest) =
  if tag == tagName
  then tagOpen : inject ++ rest
  else tagOpen : injectInto tagName inject rest
injectInto tagName inject (tag : rest) =
  tag : injectInto tagName inject rest

