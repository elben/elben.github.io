{-# LANGUAGE OverloadedStrings #-}

module Pencil.Blog
  (
    loadBlogPosts
  , blogPostUrl
  , injectTitle
  , buildTagPages
  , injectTagsEnv
  ) where

import Pencil
import Pencil.Env
import Control.Monad (liftM, foldM)
import Control.Monad.Reader (asks)
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T
import qualified System.FilePath as FP

{-|
Copyright   : (c) Elben Shira, 2017
License     : TODO
Maintainer  : Elben Shira <elbenshira@gmail.com>
Stability   : experimental

This module provides a standard way of building and generating blog posts.

Also allows pages to be tagged with tags, using the "tags" variable name in the
environment, which is expected to be an array of strings.

-}


loadBlogPosts :: FilePath -> PencilApp [Page]
loadBlogPosts fp = do
  -- Load posts
  postFps <- listDir False fp

  -- Sort by date (newest first) and filter out drafts
  liftM (filterByVar True "draft" (EBool True /=) . sortByVar "date" dateOrdering)
        (mapM (load blogPostUrl) postFps)

-- | Rewrite file path for blog posts.
-- "/blog/2011-01-01-the-post-title.html" => "/blog/the-post-title/"
blogPostUrl :: FilePath -> FilePath
blogPostUrl fp = FP.replaceFileName fp (drop 11 (FP.takeBaseName fp)) ++ "/"

injectTitle :: T.Text -> Page -> Page
injectTitle titlePrefix page =
  let title = case H.lookup "postTitle" (getPageEnv page) of
                       Just (EText t) -> T.append (T.append t " - ") titlePrefix
                       _ -> titlePrefix
      env' = insertEnvText "title" title (getPageEnv page)
  in setPageEnv env' page

type Tag = T.Text

-- | Given Pages with "tags" variables in its environments, build Pages that
-- contain in its environment the list of Pages that were tagged with that
-- particular tag.
buildTagPages :: FilePath
              -- ^ Partial to load for the Tag index pages
              -> T.Text
              -- ^ Variable name inserted into Tag index pages for the list of
              -- Pages tagged with the specified tag
              -> (Tag -> FilePath -> FilePath)
              -- ^ Function to generate the URL of the tag pages.
              -> [Page]
              -> PencilApp (H.HashMap Tag Page)
buildTagPages tagPageFp pagesVar fpf pages = do
  env <- asks getEnv

  let tagMap = groupByElements "tags" pages
  -- Build a mapping of tag to the tag list Page

  foldM
    (\acc (tag, taggedPosts) -> do
      tagPage <- load (fpf tag) tagPageFp
      let tagEnv = (insertEnvListPage pagesVar taggedPosts . insertEnvText "tag" tag . insertEnv (getPageEnv tagPage)) env
      return $ H.insert tag (setPageEnv tagEnv tagPage) acc
    )
    H.empty
    (H.toList tagMap)

injectTagsEnv :: H.HashMap Tag Page -> Page -> Page
injectTagsEnv tagMap page =
  -- Build up an env list of tag to that tag page's env. This is so that we can
  -- have access to the URL of the tag index pages.
  let tagEnvList =
        case H.lookup "tags" (getPageEnv page) of
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

      -- Overwrite the EArray "tags" variable in the post Page with EEnvList of the
      -- loaded Tag index pages. This is so that when we render the blog posts, we
      -- have access to the URL of the Tag index.
      env' = insertEnvData "tags" tagEnvList (getPageEnv page)
  in setPageEnv env' page

