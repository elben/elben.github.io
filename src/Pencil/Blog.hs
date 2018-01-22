{-# LANGUAGE OverloadedStrings #-}

module Pencil.Blog where

import Pencil
import Pencil.Env
import qualified System.FilePath as FP
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Text as T

-- | Rewrite file path for blog posts.
-- "/blog/2011-01-01-the-post-title.html" => "/blog/the-post-title/"
blogPostUrl :: FilePath -> FilePath
blogPostUrl fp = FP.replaceFileName fp (drop 11 (FP.takeBaseName fp)) ++ "/"

prepareBlogPost :: T.Text -> H.HashMap T.Text Page -> Page -> Page
prepareBlogPost titlePrefix tagMap page@(Page _ env _) =
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
                       Just (EText title) -> T.append (T.append title " - ") titlePrefix
                       _ -> titlePrefix

      -- Overwrite the EArray "tags" variable in the post Page with EEnvList of the
      -- loaded Tag index pages. This is so that when we render the blog posts, we
      -- have access to the URL of the Tag index.
      env' = (insertEnvData "tags" tagEnvList . insertEnvText "title" pageTitle) env
  in page { getPageEnv = env' }

