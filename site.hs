--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.List (isSuffixOf, find)
import           Control.Monad (filterM, liftM)
import           System.Environment (lookupEnv)
import           Hakyll

------------------------
-- Overview
------------------------
--
-- Generates static web page.
--
-- Identifier - In essence, a file path (e.g. posts/123.html, index). Can be
--              actual (the file posts/123.html exists) or virtual (you create
--              the file myblogpost.html from myblogpost.markdown).
--
-- Metadata   - Map String String. Get Metadata from an Identifier.
--
-- Item       - Some kind of content (of a file) and its Identifier. Has
--              Identifier so that you can get the metadata from the file.
--
--              For our custom Items (project items), we can ignore the
--              Identifier (use a fake one), since the Context we would use
--              doesn't need to actually load a file (none exists).
--
-- Context    - Hakyll templates uses variables like $title$ as placeholders.
--              A Context describes *how* to get the value of a field.
--
--              For example, a `Context a` is in essence, a mapping of String
--              keys to a function takes an `Item a` and returns a Compiler for
--              type `a`.
--
--              Mostly you see `Context String`. But we have, for the projects
--              listing, a `Context CustomProjectDataType`, which knows how to
--              look into a CustomProjectDataType to find the fields it needs to
--              inject into `Item CustomProjectDataType`s.
--
--              At the end of the day, a Context + Item is applied to a template
--              via methods like applyTemplate.
--
--              Check out how the `field` function, which constructs a new field
--              in the Context, takes a function of (Item a -> Compiler String):
--              http://jaspervdj.be/hakyll/reference/src/Hakyll-Web-Template-Context.html#field
--
-- Rules      - Monad DSL for declaring routes and compliers. What the `hakyll`
--              function works in.
--
-- Resources:
--
-- https://jaspervdj.be/hakyll/tutorials/a-guide-to-the-hakyll-module-zoo.html

--------------------------------------------------------------------------------
main :: IO ()
main = do
  loadDraftsEnv <- lookupEnv "LOAD_DRAFTS"
  let loadDrafts = maybe False (=="true") loadDraftsEnv
  hakyll $ do
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

    -- Load all posts. If the environemnt specifies, filter out draft posts.
    -- Create [Identifier] and Pattern, since functions differ on which one they
    -- use.
    postIds <- findAllPostIds loadDrafts
    let postsPattern = fromList postIds

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
    tags <- buildTags postsPattern (fromCapture "blog/tags/*.html")

    -- Generate a page for each tag in the Rules monad.
    --
    -- `pattern` is list of posts with the given `tag`.
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged with \"" ++ tag ++ "\""
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let ctx = constField "title" title `mappend`
                      listField "posts" postContext (return posts) `mappend`
                      defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx
                >>= relativizeUrls

    match postsPattern $ do
        route $ customRoute formatFilename
        compile $ pandocCompiler
            -- Render just the post body first, so that I can `saveSnapshot`
            -- just the body for the Atom feed.
            >>= loadAndApplyTemplate "templates/post-body.html"    (postContextWithTags tags)
            >>= saveSnapshot postContentSnapshot
            >>= loadAndApplyTemplate "templates/post.html"    (postContextWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postContextWithTags tags)
            >>= processUrls

    create ["proj.html"] $ do
        route idRoute
        compile $ do
            -- Create a list of project items ([Item (String,String,String)])
            let fakeId = fromFilePath "fake"
            let items = map (\p -> Item { itemIdentifier = fakeId, itemBody = p }) projects

            -- The `projCtx` context knows how to query into a project tuple.
            let ctx = listField "projects" projCtx (return items)

            template <- loadBody "templates/projects-list.html"
            makeItem ("" :: String) >>= applyTemplate template ctx

    create ["blog/index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllIds postIds
            let archiveCtx =
                    listField "posts" postContext (return posts) `mappend`
                    constField "title" "Elben Shira - Blog Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= processUrls

    create ["blog/atom.xml"] $ do
        route idRoute
        compile $ renderAtomFeedForPattern postsPattern

    -- For http://planet.clojure.in/, which subscribes to my feed
    create ["blog/tags/clojure.xml"] $ do
        route idRoute
        compile $ renderAtomFeedForPattern (filterByTag tags "clojure")

    -- Handle projects/index.html, which already exists
    create ["projects/index.html"] $ do
        -- Route final generated file to the same path as above
        route idRoute

        compile $ do
            let ctx = defaultContext

            getResourceBody
                >>= applyAsTemplate ctx
                >>= loadAndApplyTemplate "templates/default.html" ctx

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAllIds postIds

            recPosts <- recentFirst =<< loadAll (filterByTag tags "recommended")

            -- Build context for the template (set template variable values)
            let indexCtx =
                    listField "posts" postContext (return posts)               `mappend`
                    listField "recommendedPosts" postContext (return recPosts) `mappend`
                    constField "title" websiteTitle                            `mappend`
                    defaultContext
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= processUrls

    match "templates/*" $ compile templateCompiler

websiteTitle :: String
websiteTitle = "Elben Shira"

-- Name for snapshop that contains only the blog post body, without title or
-- other metadata.
postContentSnapshot :: String
postContentSnapshot = "content"

-- Render Atom feed for the pattern of posts.
renderAtomFeedForPattern :: Pattern -> Compiler (Item String)
renderAtomFeedForPattern pattern = do
    posts <- fmap (take 10) (recentFirst =<< loadAllSnapshots pattern postContentSnapshot)
    renderAtom atomFeedConfiguration feedContext posts

-- Use the body of the post in the 'description' field.
feedContext :: Context String
feedContext = postContext `mappend` bodyField "description"

postContext :: Context String
postContext =
    dateField "date" "%e %B %Y"            `mappend`
    defaultContext

-- `tagsField` renders tags with links. Puts it in the "tags" field context.
--
-- It gets the right tags with `tagsField`, which re-searches the Identifier to
-- get the tags for that page, then looks at the given `tags` to find the URL to
-- route it to (e.g. tags/haskell.html). See
-- http://jaspervdj.be/hakyll/reference/src/Hakyll-Web-Tags.html#tagsFieldWith
--
postContextWithTags :: Tags -> Context String
postContextWithTags tags = tagsField "tags" tags `mappend` postContext

-- Given the retrieved Tags and a tag, find all posts that contain the tag.
-- Returns a Pattern list of posts.
filterByTag :: Tags -> String -> Pattern
filterByTag tags tag = case find (\(tag', _) -> tag' == tag) (tagsMap tags) of
                         Just (_, identifiers) -> fromList identifiers
                         Nothing               -> fromList []

-- liftM promotes the `maybe ...` function into the MonadMetadata monad.
-- liftM :: Monad m => (a1 -> r) -> (m a1 -> m r)
isNotDraft :: MonadMetadata m => Identifier -> m Bool
isNotDraft i = liftM (maybe True (/= "true")) (getMetadataField i "draft")
-- Equivalent:
-- isNotDraft i = getMetadataField i "draft" >>= return . (maybe True (/="true"))

findAllPostIds :: MonadMetadata m
               => Bool
               -- ^ Include drafts if true.
               -> m [Identifier]
findAllPostIds includeDrafts = do
  ids <- getMatches "posts/*"
  if includeDrafts then return ids else filterM isNotDraft ids

loadAllIds :: [Identifier] -> Compiler [Item String]
loadAllIds = mapM load

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

atomFeedConfiguration :: FeedConfiguration
atomFeedConfiguration = FeedConfiguration
    { feedTitle       = websiteTitle
    , feedDescription = ""
    , feedAuthorName  = "Elben Shira"
    , feedAuthorEmail = "elbenshira@gmail.com"
    , feedRoot        = "http://elbenshira.com"
    }

projects :: [(String, String, String)]
projects =
  [ ("My project", "https://github.com/elben/", "This is my sweet project")
  , ("My project second", "https://github.com/elben/", "This is my sweet project second one")
  ]

projNameCtx :: Context (String,a,b)
projNameCtx = field "name" $ \item -> do
    let (name, _, _) = itemBody item
    return name

projUrlCtx :: Context (a,String,b)
projUrlCtx = field "url" $ \item -> do
    let (_, url, _) = itemBody item
    return url

projDescriptionCtx :: Context (a,b,String)
projDescriptionCtx = field "description" $ \item -> do
    let (_, _, description) = itemBody item
    return description

-- A context that knows how to query into the project tuple. So if the context
-- is paired with an Item (String,String,String), it can grab the elements it
-- needs out of the tuple.
projCtx :: Context (String,String,String)
projCtx = projNameCtx `mappend` projUrlCtx `mappend` projDescriptionCtx

