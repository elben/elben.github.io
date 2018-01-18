{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Pencil.Env
import Control.Monad (forM_, foldM, liftM, (>=>))
import Data.List.NonEmpty (NonEmpty(..)) -- Import the NonEmpty data constructor, (:|)
import Control.Monad.Reader (runReaderT, asks)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.FilePath as FP

-- | Rewrite file path for blog posts.
-- "/blog/2011-01-01-the-post-title.html" => "/blog/the-post-title/"
blogPostUrl :: FilePath -> FilePath
blogPostUrl fp = FP.replaceFileName fp (drop 11 (FP.takeBaseName fp)) ++ "/"

prepareBlogPost :: H.HashMap T.Text Page -> Page -> Page
prepareBlogPost tagMap page@(Page _ env _) =
  let tagEnvList =
        case H.lookup "tags" env of
          Just (EArray tags) ->
            EEnvList $
              L.foldl'
                (\acc envData ->
                  case envData of
                    EText tag ->
                      case H.lookup tag tagMap of
                        Just tagIndexPage -> getPageEnv tagIndexPage : acc
                        _ -> acc
                    _ -> acc)
                [] tags
          _ -> EEnvList []
      pageTitle = case H.lookup "postTitle" env of
                       Just (EText title) -> T.append (T.append title " - ") websiteTitle
                       _ -> websiteTitle

      -- Overwrite the EArray "tags" variable in the post Page with EEnvList of the
      -- loaded Tag index pages. This is so that when we render the blog posts, we
      -- have access to the URL of the Tag index.
      env' = (insertEnvData "tags" tagEnvList . insertEnvText "title" pageTitle) env
  in page { getPageEnv = env' }

websiteTitle :: T.Text
websiteTitle = "Elben Shira"

config :: Config
config = Config {
    cSitePrefix = "site/"
  , cOutPrefix = "out/"
  , cEnv = H.fromList [("title", EText websiteTitle)]
}

main :: IO ()
main =
  runReaderT app config

app :: PencilApp ()
app = do
  pageLayout <- liftM forceRight (loadPageAsHtml "layouts/default.html")
  pagePartial <- liftM forceRight (loadPageAsHtml "partials/post.html")

  -- Load posts
  postFps <- listDir False "blog/"

  -- Sort by date (newest first) and filter out drafts
  posts <- liftM (filterByVar True "draft" (EBool True /=) . sortByVar "date" dateOrdering)
                 (mapM (liftM forceRight . loadPageWithFileModifier blogPostUrl) postFps)

  let recommendedPosts = filterByVar False "tags"
                           (arrayContainsString "recommended")
                           posts

  env <- asks cEnv

  -- Tags and tag list pages

  let tagMap = groupByTagVar "tags" posts
  -- Build a mapping of tag to the tag list Page
  tagPages <- foldM
    (\acc (tag, taggedPosts) -> do
      tagPage <- liftM forceRight $ loadPageWithFileModifier (const ("blog/tags/" ++ T.unpack tag ++ "/")) "partials/post-list-for-tag.html"
      let tagEnv = (insertEnvListPage "posts" taggedPosts . insertEnvText "tag" tag . insertEnv (getPageEnv tagPage)) env
      return $ H.insert tag (tagPage { getPageEnv = tagEnv }) acc
    )
    H.empty
    (H.toList tagMap)

  -- Prepare blog posts. Add tag info into each blog post page, and then inject
  -- into the correct structure.
  let posts' = map (structurePage (pagePartial :| [pageLayout]) . prepareBlogPost tagPages) posts

  -- Render blog posts
  forM_ posts' (applyPage env >=> renderPage)

  -- Index
  -- Function composition
  let postsEnv = (insertEnvListPage "posts" posts . insertEnvListPage "recommendedPosts" recommendedPosts) env
  indexPage <- liftM forceRight $ loadPageAsHtml "index.html"
  applyPage postsEnv (indexPage :| [pageLayout]) >>= renderPage

  -- Render tag list pages
  forM_ (H.elems tagPages) (\page -> applyPage env (page :| [pageLayout]) >>= renderPage)

  -- Render blog post archive
  archivePage <- liftM forceRight $ loadPageWithFileModifier (const "blog/") "partials/post-archive.html"
  let postsArchiveEnv = insertEnvListPage "posts" posts env
  applyPage postsArchiveEnv (archivePage :| [pageLayout]) >>= renderPage

  -- /projects/
  projectsPage <- liftM forceRight $ loadPageWithFileModifier (const "projects/") "projects.html"
  applyPage env (projectsPage :| [pageLayout]) >>= renderPage

  -- Render CSS file
  renderCss "stylesheets/default.scss"

  -- Render static directories
  loadDirWithFileModifier True False fileModifierToHtml "p/" >>= renderResources
  loadDirId True True "stylesheets/fonts/" >>= renderResources
  loadDirId True True "images/" >>= renderResources

  loadResourceId "CNAME" >>= renderResource

