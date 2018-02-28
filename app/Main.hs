{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Pencil.Blog
import Pencil.Internal.Env
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

websiteTitle :: T.Text
websiteTitle = "Elben Shira"

config :: Config
config = setEnv
           (H.fromList [("title", VText websiteTitle)])
           defaultConfig

main :: IO ()
main =
  run app config

app :: PencilApp ()
app = do
  layoutPage <- load toHtml "partials/default-layout.html"
  postPage <- load toHtml "partials/post.html"

  posts <- loadBlogPosts "blog/"

  let recommendedPosts = filterByVar False "tags"
                           (arrayContainsString "recommended")
                           posts

  env <- asks getEnv

  -- Tags and tag list pages

  -- Build a mapping of tag to the tag list Page
  tagPages <- buildTagPages "partials/post-list-for-tag.html" posts

  -- Prepare blog posts. Add tag info into each blog post page, and then inject
  -- into the correct structure.
  let posts' = map ((layoutPage <|| postPage <|) . injectTagsEnv tagPages . injectTitle websiteTitle) posts

  -- Render blog posts
  render posts'

  -- Index
  -- Function composition
  let indexEnv = (insertPages "posts" posts . insertPages "recommendedPosts" recommendedPosts) env
  indexPage <- load toHtml "index.html"
  withEnv indexEnv (render (layoutPage <|| indexPage))

  -- Render tag list pages
  forM_ (H.elems tagPages) (\page -> render (layoutPage <|| page))

  -- Render blog post archive
  archivePage <- load (const "blog/") "partials/post-archive.html"
  let archiveEnv = insertPages "posts" posts env
  withEnv archiveEnv (render (layoutPage <|| archivePage))

  -- /projects/
  projectsPage <- load (const "projects/") "projects.html"
  render (layoutPage <|| projectsPage)

  -- Render CSS file
  renderCss "stylesheets/default.scss"

  -- Render static directories
  loadResources toExpected True False "p/" >>= render
  loadResources id True True "stylesheets/fonts/" >>= render
  loadResources id True True "images/" >>= render

  passthrough "CNAME" >>= render

  pencilWebsite


pencilWebsite :: PencilApp ()
pencilWebsite = do
  renderCss "pencil/default.css"
  layout <- load toHtml "pencil/layout.html"

  index <- load toHtml "pencil/index.markdown"
  render (layout <|| index)

  t1 <- load toDir "pencil/tutorials/01-getting-started.markdown"
  render (layout <|| t1)

  t2 <- load toDir "pencil/tutorials/02-deploying-to-github-pages-using-circle.markdown"
  render (layout <|| t2)

