module Pencil.Env where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Time.Clock as TC
import qualified Data.Time.Format as TF
import qualified Text.HTML.TagSoup as TS

type Tags = [TS.Tag T.Text]

-- We should use that hack that allows ppl to extend this with their own types?
-- Example: we want a "tags" type for a list of blog post tags
-- https://two-wrongs.com/dynamic-dispatch-in-haskell-how-to-make-code-extendable
data EnvData =
  EText T.Text
  | EDateTime TC.UTCTime
  | EHtml Tags
  | EList [Env]
  deriving (Eq, Show)

type Env = H.HashMap T.Text EnvData

envDataToDisplay :: EnvData -> T.Text
envDataToDisplay (EText t) = t
envDataToDisplay (EHtml tags) = TS.renderTags tags
envDataToDisplay (EList envs) = T.unwords $ map (T.unwords . map envDataToDisplay . H.elems) envs
envDataToDisplay (EDateTime dt) =
  -- December 30, 2017
  T.pack $ TF.formatTime TF.defaultTimeLocale "%B %e, %Y" dt

-- | Accepted format is ISO 8601 (YYYY-MM-DD), optionally with an appended "THH:MM:SS".
-- Example: 2010-01-30, 2010-01-30T09:08:00
toDateTime :: String -> Maybe TC.UTCTime
toDateTime s =
  case TF.parseTimeM True TF.defaultTimeLocale (TF.iso8601DateFormat Nothing) s of
    Nothing -> TF.parseTimeM True TF.defaultTimeLocale (TF.iso8601DateFormat (Just "%H:%M:%S")) s
    Just dt -> Just dt

