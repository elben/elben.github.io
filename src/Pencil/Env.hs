module Pencil.Env where

import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS

type Tags = [TS.Tag T.Text]

-- We should use that hack that allows ppl to extend this with their own types?
-- Example: we want a "tags" type for a list of blog post tags
-- https://two-wrongs.com/dynamic-dispatch-in-haskell-how-to-make-code-extendable
data EnvData =
  EText T.Text
  | EHtml Tags
  | EList [Env]
  deriving (Eq, Show)

type Env = H.HashMap T.Text EnvData

envDataToDisplay :: EnvData -> T.Text
envDataToDisplay (EText t) = t
envDataToDisplay (EHtml tags) = TS.renderTags tags
