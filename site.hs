--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Data.List (isSuffixOf)
import           Hakyll
import           Debug.Trace

------------------------
-- Overview
------------------------
--
-- Generates static web page.
--
-- Identifier - In essence, a file path (e.g. posts/123.html, index). Can be
--              actual (the file posts/123.html exists) or virtual (you create
--              the file 'index').
--
-- Metadata   - Map String String. Get Metadata from an Identifier.
--
-- Item       - Some kind of content and its Identifier. Has Identifier so that
--              you can get the metadata.
--
-- Context    - In essence, a mapping of String keys to content. These are the
--              variables in the template.
--
--              Contexts by themselves are not paired with Items. This happens
--              later when we need the field values. Check out how the `field`
--              function, which constructs a new field in the Context, takes a
--              function of (Item a -> Compiler String):
--              http://jaspervdj.be/hakyll/reference/src/Hakyll-Web-Template-Context.html#field
--
-- Rules      - Monad DSL for declaring routes and compliers.
--
-- Resources:
--
-- http://jaspervdj.be/hakyll/tutorials/a-guide-to-the-hakyll-module-zoo.html

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do

    ------------------------
    -- Static
    ------------------------

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/fonts/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "p/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    ------------------------
    -- Dynamic
    ------------------------

    -- `Tags` contains:
    -- * tagsMap    - A list of tag strings paired with Identifiers it was found on
    -- * tagsMakeId - A function to convert a tag (String) to an Identifier
    --   (some *new* path for the canonical URL of a tag, e.g. tags/haskell.html).
    --
    -- Search metadata in blob Pattern for tags.
    --
    -- `(fromCapture ...)` expression returns function that fills in the `*` in the
    -- capture, given a string.
    --
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Generate a page for each tag in the Rules monad.
    --
    -- `pattern` is list of posts with the given `tag`.
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                      listField "posts" postContext (return posts) `mappend`
                      defContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match "posts/*" $ do
        route $ customRoute formatFilename
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postContextWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postContextWithTags tags)
            >>= processUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postContext (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= processUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postContext (return posts) `mappend`
                    constField "title" "Elben Shira"         `mappend`
                    defContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= processUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postContext :: Context String
postContext =
    dateField "date" "%e %B %Y"            `mappend`
    defContext

-- `tagsField` renders tags with links. Puts it in the "tags" field context.
--
-- It gets the right tags with `tagsField`, which re-searches the Identifier to
-- get the tags for that page, then looks at the given `tags` to find the URL to
-- route it to (e.g. tags/haskell.html). See
-- http://jaspervdj.be/hakyll/reference/src/Hakyll-Web-Tags.html#tagsFieldWith
--
postContextWithTags :: Tags -> Context String
postContextWithTags tags = tagsField "tags" tags `mappend` postContext

homeContext :: Context String
homeContext = constField "site_header_class" "" `mappend` defContext

defContext :: Context String
defContext =
  constField "site_header_class" "small" `mappend` defaultContext

filterByTag :: String -> [Item String] -> Compiler [Item String]
-- filterByTag tag items = return $ filter (\item -> getTags (itemIdentifier item) >>= (\s -> True)) items
filterByTag tag items = return items

-- TODO fix to do better
-- From "posts/yyyy-mm-dd-post-title.markdown" to "blog/post-title/index.html"
formatFilename :: Identifier -> String
formatFilename ident = "blog/" ++ takeWhile (/= '.') (drop 17 (toFilePath ident)) ++ "/index.html"

processUrls :: Item String -> Compiler (Item String)
processUrls i = relativizeUrls i >>= cleanIndexUrls

-- Original from https://groups.google.com/forum/#!topic/hakyll/s1SgkIzRdMQ
--
-- Strips "index.html" from non-external URLs in Item.
cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls clean)
  where
    idx = "index.html"
    clean url
        | idx `isSuffixOf` url && (not . isExternal) url = take (length url - length idx) url
        | otherwise = url

