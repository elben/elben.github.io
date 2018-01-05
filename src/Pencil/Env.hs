{-# LANGUAGE OverloadedStrings #-}

module Pencil.Env where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Time.Clock as TC
import qualified Data.Time.Format as TF
import qualified Text.HTML.TagSoup as TS
import qualified Data.Aeson as A
import qualified Data.Vector as V

type Tags = [TS.Tag T.Text]

-- We should use that hack that allows ppl to extend this with their own types?
-- Example: we want a "tags" type for a list of blog post tags
-- https://two-wrongs.com/dynamic-dispatch-in-haskell-how-to-make-code-extendable
--
-- Represents the data types found in an environment. This includes at least
-- Data.Aeson Value type
-- (https://hackage.haskell.org/package/aeson-1.2.3.0/docs/Data-Aeson.html#t:Value),
-- plus other useful ones.
data EnvData =
    EText T.Text
  | EBool Bool
  | EDateTime TC.UTCTime
  | EArray [EnvData]
  | EEnvList [Env]
  deriving (Eq, Show)

type Env = H.HashMap T.Text EnvData

toEnvData :: A.Value -> Maybe EnvData
toEnvData (A.Bool b) = Just $ EBool b
toEnvData (A.String s) =
  -- See if coercible to datetime
  case toDateTime (T.unpack s) of
    Nothing -> Just $ EText s
    Just dt -> Just $ EDateTime dt
toEnvData (A.Array arr) =
  Just $ EArray (V.toList (V.mapMaybe toEnvData arr))
toEnvData _ = Nothing

envDataToDisplay :: EnvData -> T.Text
envDataToDisplay (EText t) = t
envDataToDisplay (EArray arr) = T.unwords $ map envDataToDisplay arr
envDataToDisplay (EBool b) = if b then "true" else "false"
envDataToDisplay (EEnvList envs) = T.unwords $ map (T.unwords . map envDataToDisplay . H.elems) envs
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

