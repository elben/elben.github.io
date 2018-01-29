module Pencil
  (
    -- * Getting started
    --
    -- $gettingstarted

    -- * Templates
    --
    -- $templates

    PencilApp
  , run

  -- * Pages, Structures and Resources
  --
  -- $pagesStructuresResources

  , Page
  , getPageEnv, setPageEnv
  , load
  , withEnv
  , renderCss

  , Structure
  , (<||)
  , (<|)
  , structure

  , Resource
  , loadResource
  , loadResources
  , passthrough
  , listDir

  , Render(..)
  , asHtml
  , asDir
  , asCss
  , asIntended

  -- * Environment Manipulation

  , merge
  , modifyEnv
  , insertEnv
  , insertText
  , insertPages
  , sortByVar
  , filterByVar
  , groupByElements

  -- * Configuration

  , Config
  , defaultConfig
  , getSourceDir, setSourceDir
  , getOutputDir, setOutputDir
  , getEnv, setEnv
  , getSassOptions, setSassOptions
  , getMarkdownOptions, setMarkdownOptions

  -- * Utils

  , FileType
  , fileType
  , toExtension

  -- * Error handling

  , PencilException

  ) where

import Pencil.Internal


----------------------------------------------------------------------

-- $gettingstarted
--
-- We'll start by building a very simple website, with only a couple of pages,
-- to give you a feel for using Pencil. Go to http://elbenshira.com/pencil for
-- in-depth tutorials, including how you can set up a blog.
--
-- To build our simple website, we'll first create some folders and files:
--
-- > cd ~/code/mywebsite
-- > mkdir site/
-- > echo '
-- > <html>
-- >   <head>
-- >     <title>${title}</title>
-- >     <link rel="stylesheet" href="style.css"/>
-- >   </head>
-- > <body>${body}</body>
-- > </html>' > site/layout.html
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
--   layout <- load asHtml "layout.html"
--   index <- load asHtml "index.markdown"
--
--   render (layout <|| index)
--   renderCss "style.scss"
--
-- main = run website config
-- @

----------------------------------------------------------------------

-- $templates
--
-- Pencil comes with a simple templating engine. Templates allow us to build web
-- pages dynamically using Haskell code. This allows us to build modular
-- components. Templates can be used for things like shared page layouts,
-- navigation and blog post templates.
--
-- Pencil templates are regular text files that can contain a /preamble/ and
-- /directives/.
--
-- == Preamble
--
-- == Directives
--
-- Directives are rendering /commands/. They are surrounded by @${...}@.
--
-- === Variables
--
-- The simplest directive is the variable directive.
--
-- @
-- Hello ${name}!
-- @
--
-- The above template will render the value of the variable @name@, which is
-- expected to be in the environment at 'render'. If the variable is not found,
-- the final web page will show @${name}@ as-is, to help you in debugging
-- missing variables.
--
-- === If block
--
-- The @if@ directive allows us to render content based off the existence of a
-- variable in the current environment.
--
-- > ${if(name)}
-- >   Hello ${name}!
-- > ${end}
--
-- In this case, we now make sure that @name@ is available before rendering.
--
-- === For loop
--
-- The @for@ directive allows us to loop over array type variable. This is
-- useful for things like rendering a list of blog post titles, and URLs to the
-- individual blog posts.
--
-- > <ul>
-- > ${for(posts)}
-- >   <li><a href="${this.url}">${postTitle}</a> - ${date}</li>
-- > ${end}
-- > </ul>
--
-- Assuming that @posts@ exists in our environment as an array of @Value@,
-- this will render each post's title, publish date, and will link it to
-- @this.url@. Note that inside the @for@ block, you have access to the current
-- environment's variables. This is why we're able to simply request
-- @${postTitle}@—it is the current post's @postTitle@ that will be rendered.
--
-- @this.url@ is a special variable that is automatically inserted for you
-- inside a loaded @Page@. It points to the final file path destination of that
-- current @Page@.
--
-- === Partials

----------------------------------------------------------------------

-- $pagesStructuresResources
--
-- 'Page', 'Structure' and 'Resource' are the "big three" data types you need to
-- know to effectively use Pencil.

----------------------------------------------------------------------
