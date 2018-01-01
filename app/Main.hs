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

doPage :: Env -> Page -> IO ()
doPage env (Page nodes penv fp) = do
  layoutText <- TIO.readFile "site/layouts/default.html"
  let layoutTags = injectIntoHead (cssTag "/stylesheets/default.css") (TS.parseTags layoutText)

  let env' = H.union env penv
  let nodes' = replaceVarsInTemplate env' nodes
  Page partialNodes partialEnv _ <- loadPage' "partials/post.html"

  -- Insert an Aeson string as the "body" variable. Prefer LHS keys for dupes.
  let env'' = H.union (H.insert "body" (EText (renderNodes nodes')) env') partialEnv

  let partialNodes' = replaceVarsInTemplate env'' partialNodes

  let env''' = H.insert "body" (EHtml (TS.parseTags (renderNodes partialNodes'))) env''
  let layoutTextForPost1 = replaceVarsInText env''' (TS.renderTags layoutTags)

  TIO.writeFile (outPrefix ++ fp ++ ".html") layoutTextForPost1

main :: IO ()
main = do
  -- Layout
  layoutText <- TIO.readFile "site/layouts/default.html"
  let layoutTags = injectIntoHead (cssTag "/stylesheets/default.css") (TS.parseTags layoutText)

  -- Index
  Page indexNodes indexEnv _ <- loadPage' "index.html"
  let indexTags = injectIntoBodyVar (TS.parseTags (renderNodes indexNodes)) layoutTags
  let indexNodesReplaced = replaceVarsInText (H.union globalEnv indexEnv) (TS.renderTags indexTags)
  TIO.writeFile "out/index.html" indexNodesReplaced

  -- Write CSS file
  includeAsset "stylesheets/mysheet.css"
  includeAsset "stylesheets/default.css"

  -- Load posts
  pages <- mapM loadPage'
                  [ "posts/2010-01-30-behind-pythons-unittest-main.markdown"
                  , "posts/2010-04-16-singleton-pattern-in-python.markdown"
                  ]

  forM_ pages (doPage globalEnv)

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

data Page = Page
  { getPageNodes    :: [PNode]
  , getPageEnv      :: Env
  , getPageFilePath :: String
  -- ^ The original filename, without the extension and without site prefix path
  } deriving (Eq, Show)

-- | Load page, extracting the tags and preamble variables. Renders Markdown
-- files into HTML.
loadPage' :: FilePath -> IO Page
loadPage' fp = do
  -- foo/bar/file.markdown -> foo/bar/file
  content <- TIO.readFile (sitePrefix ++ fp)
  let fp' = FP.dropExtension fp
  let extension = FP.takeExtension fp
  let env = aesonToEnv $ loadVariables (TS.parseTags content)
  let content' =
        if extension `elem` [".markdown", ".md"]
        then CM.commonmarkToHtml [] content
        else content
  case runParser content' of
    Left _ -> return $ Page [] env fp'
    Right nodes -> return $ Page nodes env fp'

-- cleanFilePath :: FilePath -> FilePath
-- cleanFilePath fp =

-- | Load page, extracting the tags and preamble variables. Renders Markdown
-- files into HTML.
loadPage :: FilePath -> IO ([PNode], Env)
loadPage fp = do
  content <- TIO.readFile fp
  let env = aesonToEnv $ loadVariables (TS.parseTags content)
  let content' =
        if ".markdown" `DL.isSuffixOf` fp
        then CM.commonmarkToHtml [] content
        else content
  case runParser content' of
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

