{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pencil
import Control.Monad (forM_)
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T

websiteTitle :: T.Text
websiteTitle = "Elben Shira"

config :: Config
config = updateEnv (insertText "title" websiteTitle) defaultConfig

main :: IO ()
main =
  run app config

app :: PencilApp ()
app = do
  layoutPage <- load "partials/default-layout.html"
  postPage <- load "partials/post.html"

  posts <- loadPosts "blog/"

  let recommendedPosts = filterByVar False "tags"
                           (arrayContainsText "recommended")
                           posts

  env <- asks getEnv

  -- Tags and tag list pages

  -- Build a mapping of tag to the tag list Page
  tagPages <- buildTagPages "partials/post-list-for-tag.html" posts

  -- Prepare blog posts. Add tag info into each blog post page, and then inject
  -- into the correct structure.
  let posts' = map ((layoutPage <|| postPage <|) . injectTags tagPages . injectTitle websiteTitle) posts

  -- Render blog posts
  render posts'

  -- Index
  -- Function composition
  indexPage <- load "index.html"
  indexEnv <- (insertPages "posts" posts env >>= insertPages "recommendedPosts" recommendedPosts)
  withEnv indexEnv (render (layoutPage <|| indexPage))

  -- Render tag list pages
  forM_ (H.elems tagPages) (\page -> render (layoutPage <|| page))

  -- Render blog post archive
  archivePage <- load "partials/post-archive.html"
  render (layoutPage <|| to "blog/" archivePage <<| coll "posts" posts)

  -- RSS feed for blog posts
  rssLayout <- move "blog/" <$> load "rss.xml"

  -- Build RSS feed of the last 5 posts.
  let rssFeedStruct = struct rssLayout <<| coll "posts" (fmap escapeXml (take 5 posts))

  -- Render the RSS feed. We need to render inside a modified environment, where
  -- @toTextRss@ is used as the render function, so that dates are rendered in
  -- the RFC 822 format, per the RSS specification.
  local (setDisplayValue toTextRss)
        (render rssFeedStruct)

  -- /projects/
  projectsPage <- load "projects.html"
  render (layoutPage <|| to "projects/" projectsPage)

  -- Render CSS file
  loadAndRender "stylesheets/default.scss"

  -- Render static directories
  loadAndRender "p/"
  loadAndRender "stylesheets/fonts/"
  loadAndRender "images/"

  passthrough "CNAME" >>= render