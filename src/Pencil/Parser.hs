{-# LANGUAGE OverloadedStrings #-}

module Pencil.Parser where

import Text.ParserCombinators.Parsec
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS

-- Doctest setup.
--
-- $setup
-- >>> import Data.Either (isLeft)

-- | Data structure that mirrors the tagsoup @Tag@ data. We need more data types for our variables.
data PTag =
  PTagOpen T.Text [TS.Attribute T.Text]
  | PTagClose T.Text
  | PTagText T.Text
  | PTagComment T.Text
  | PTagVariable T.Text

parsePTags :: T.Text -> [PTag]
parsePTags t = fromTagsoup (TS.parseTags t)

fromTagsoup :: [TS.Tag T.Text] -> [PTag]
fromTagsoup = map fromTagsoup'

fromTagsoup' :: TS.Tag T.Text -> PTag
fromTagsoup' (TS.TagOpen t attrs) = PTagOpen t attrs
fromTagsoup' (TS.TagClose t) = PTagClose t
fromTagsoup' (TS.TagText t) = PTagText t
fromTagsoup' (TS.TagComment t) = PTagComment t

toTagsoup :: [PTag] -> [TS.Tag T.Text]
toTagsoup = map toTagsoup'

toTagsoup' :: PTag -> TS.Tag T.Text
toTagsoup' (PTagOpen t attrs) = TS.TagOpen t attrs
toTagsoup' (PTagClose t) = TS.TagClose t
toTagsoup' (PTagText t) = TS.TagText t
toTagsoup' (PTagComment t) = TS.TagComment t
toTagsoup' (PTagVariable t) = TS.TagText (T.append "${" (T.append t "}"))

data SimpleTemplate =
    STText T.Text
  | STVar T.Text
  deriving (Show, Eq)

-- | Parse everything.
--
-- >>> parse parseEverything "" "Hello ${man} and ${woman}."
-- Right [STText "Hello ",STVar "man",STText " and ",STVar "woman",STText "."]
--
-- >>> parse parseEverything "" "<b>this $fakevar works</b> ${realvar}"
-- Right [STText "<b>this $fakevar works</b>"]
--
-- >>> parse parseEverything "" "<b>this ${fakevar works</b> ${realvar}"
-- Right [STText "<b>this ${fakevar works</b>"]
--
parseEverything :: Parser [SimpleTemplate]
parseEverything = many1 (parseContent <|> parseVar <|> parseFakeVar)

-- | Parse variables.
--
-- >>> parse parseVar "" "${ffwe} yep"
-- Right (STVar "ffwe")
--
-- >>> isLeft $ parse parseVar "" "Hello ${name}"
-- True
parseVar :: Parser SimpleTemplate
parseVar = try $ do
  _ <- char '$'
  _ <- char '{'
  varName <- many (noneOf "}")
  _ <- char '}'
  return $ STVar (T.pack varName)

-- | Parse boring, boring text.
--
-- >>> parse parseContent "" "hello ${ffwe} you!"
-- Right (STText "hello ")
--
-- >>> isLeft $ parse parseContent "" "${name}!!"
-- True
--
-- https://stackoverflow.com/questions/20730488/parsec-read-text-ended-by-a-string
-- https://github.com/jaspervdj/hakyll/blob/32e34f435c7911f36acdf4a62eec1f56faf0b269/src/Hakyll/Web/Template/Internal/Element.hs#L137
parseContent :: Parser SimpleTemplate
parseContent = do
  -- stuff <- manyTill anyChar (try (string "${"))
  -- stuff <- try (manyTill anyChar parseVar) -- <|> (many1 anyChar)
  stuff <- many1 (noneOf "$")
  return $ STText (T.pack stuff)

-- | A hack to capture strings that "almost" are templates. I couldn't figure
-- out another way.
parseFakeVar :: Parser SimpleTemplate
parseFakeVar = do
  _ <- char '$'
  n <- noneOf "{"
  rest <- many1 (noneOf "$")
  return $ STText (T.pack ("$" ++ [n] ++ rest))
