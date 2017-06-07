{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.HTML.TagSoup as TS
import qualified System.Directory as D
import qualified CMark as CM

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
  blogPost1 <- TIO.readFile "site/posts/2010-01-30-behind-pythons-unittest-main.markdown"
  blogPost2 <- TIO.readFile "site/posts/2010-04-16-singleton-pattern-in-python.markdown"
  let blogPostHtml1 = CM.commonmarkToHtml [] blogPost1
  let blogPostHtml2 = CM.commonmarkToHtml [] blogPost2
  TIO.writeFile "out/posts/2010-01-30-behind-pythons-unittest-main.html" blogPostHtml1
  TIO.writeFile "out/posts/2010-04-16-singleton-pattern-in-python.markdown" blogPostHtml2

-- The to/from location of the static asset
data StaticAsset = StaticAsset FilePath FilePath

includeAsset :: FilePath -> IO ()
includeAsset f = D.copyFile (sitePrefix ++ f) (outPrefix ++ f)

-- Type alias
type Tags = [TS.Tag T.Text]

cssTag :: T.Text -> Tags
cssTag file = TS.parseTags $ T.append "<link rel=\"stylesheet\" href=\"" $ T.append file "\" />"

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


gotoBodyStart :: [TS.Tag T.Text] -> T.Text -> [TS.Tag T.Text]
gotoBodyStart tags text = tags
