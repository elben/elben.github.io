{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Pencil.Env
import Pencil.Blog
import Control.Monad (forM_, foldM, liftM)
import Control.Monad.Reader (asks)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

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
  run app config

app :: PencilApp ()
app = do
  pageLayout <- loadHtml "layouts/default.html"
  pagePartial <- loadHtml "partials/post.html"

  -- Load posts
  postFps <- listDir False "blog/"

  -- Sort by date (newest first) and filter out drafts
  posts <- liftM (filterByVar True "draft" (EBool True /=) . sortByVar "date" dateOrdering)
                 (mapM (load blogPostUrl) postFps)

  let recommendedPosts = filterByVar False "tags"
                           (arrayContainsString "recommended")
                           posts

  env <- asks cEnv

  -- Tags and tag list pages

  let tagMap = groupByTagVar "tags" posts
  -- Build a mapping of tag to the tag list Page
  tagPages <- foldM
    (\acc (tag, taggedPosts) -> do
      tagPage <- load (const ("blog/tags/" ++ T.unpack tag ++ "/")) "partials/post-list-for-tag.html"
      let tagEnv = (insertEnvListPage "posts" taggedPosts . insertEnvText "tag" tag . insertEnv (getPageEnv tagPage)) env
      return $ H.insert tag (tagPage { getPageEnv = tagEnv }) acc
    )
    H.empty
    (H.toList tagMap)

  -- Prepare blog posts. Add tag info into each blog post page, and then inject
  -- into the correct structure.
  let posts' = map (structurePage (pageLayout <|| pagePartial) . prepareBlogPost websiteTitle tagPages) posts

  -- Render blog posts
  forM_ posts' (render env)

  -- Index
  -- Function composition
  let postsEnv = (insertEnvListPage "posts" posts . insertEnvListPage "recommendedPosts" recommendedPosts) env
  indexPage <- loadHtml "index.html"
  render postsEnv (pageLayout <|| indexPage)

  -- Render tag list pages
  forM_ (H.elems tagPages) (\page -> render env (pageLayout <|| page))

  -- Render blog post archive
  archivePage <- load (const "blog/") "partials/post-archive.html"
  let postsArchiveEnv = insertEnvListPage "posts" posts env
  render postsArchiveEnv (pageLayout <|| archivePage)

  -- /projects/
  projectsPage <- load (const "projects/") "projects.html"
  render env (pageLayout <|| projectsPage)

  -- Render CSS file
  renderCss "stylesheets/default.scss"

  -- Render static directories
  loadDirWithFileModifier True False fileModifierToHtml "p/" >>= renderResources
  loadDirId True True "stylesheets/fonts/" >>= renderResources
  loadDirId True True "images/" >>= renderResources

  loadResourceId "CNAME" >>= renderResource

