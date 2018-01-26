module Pencil
  (
    -- * Getting started
    --
    -- $gettingstarted

    PencilApp
  , run

  , Config
  , defaultConfig
  , getSourceDir, setSourceDir
  , getOutputDir, setOutputDir
  , getEnv, setEnv
  , getSassOptions, setSassOptions
  , getMarkdownOptions, setMarkdownOptions

  , Page
  , getPageEnv, setPageEnv
  , getPageFilePath, setPageFilePath
  , apply
  -- apply_
  -- , loadTextFile
  , Resource
  , renderResource
  , renderResources
  -- , copyFile
  , asHtml
  , asDir
  , markdownAsHtml
  , loadResource
  , loadResourceId
  -- , renderPage
  , render
  , load
  , loadId
  , loadDirId
  , loadDir
  , listDir
  -- , listDir_
  , renderCss

  -- , findEnv
  -- , maybeInsertIntoEnv
  -- , aesonToEnv
  -- , findPreambleText
  -- , isPreamble
  -- , preambleText

  , Structure
  , (<||)
  , (<|)
  , toStructure

  -- , mostSimilarFile

  , PencilException
  -- , toNotTextFileException
  -- , isInvalidByteSequence
  -- , isNoSuchFile

  , FileType
  -- , extensionMap
  , toExtension
  -- , markdownWriterOptions

  -- , parseTextFile
  -- , evalNodes

  , modifyEnvVar
  , sortByVar
  , filterByVar
  , groupByElements
  , insertEnv
  , insertEnvData
  , insertEnvText
  , insertEnvListPage

  , withEnv

  ) where

import Pencil.Internal


----------------------------------------------------------------------

-- $gettingstarted
--
-- We'll start by building a very simple website, with only a couple of pages, to
-- give you a feel for using Pencil. Go to http://elbenshira.com/pencil for more
-- in-depth tutorials like how to set up a blog.
--
-- To build our simple website, we'll first create some folders and files:
--
-- > cd ~/code/mywebsite
-- > mkdir site/
-- > echo '<html><head><title>${title}</title><link rel='stylesheet" href="style.css" /></head></html><body>${body}</body>" > site/layout.html
-- > echo 'Welcome to my *awesome* [website](http://example.com)!' > site/index.markdown
-- > echo '$mycolor: #ff0000; body { color: $mycolor; }' > site/style.scss
--
-- Notice that @layout.html@ contains two variable directives, @${title}@ and
-- @${body}@, which we will have to fill values for before rendering the pages that
-- use @layout.html@.
--
-- Then inside your @Main.hs@:
--
-- @
-- import Pencil
--
-- config :: Config
-- config = insertEnvText "title" "My Website" defaultConfig
--
-- website :: PencilApp ()
-- website = do
--   env <- asks getEnv
--
--   layout <- load asHtml "layout.html"
--   index <- load asHtml "index.markdown"
--
--   render env (layout <|| index)
--   renderCss "style.scss"
--
-- main = run website config
-- @

----------------------------------------------------------------------

