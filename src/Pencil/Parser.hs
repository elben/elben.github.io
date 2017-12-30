{-# LANGUAGE OverloadedStrings #-}

module Pencil.Parser where

import Text.ParserCombinators.Parsec
import qualified Data.Text as T
import qualified Text.HTML.TagSoup as TS
import qualified Data.List as L

-- Doctest setup.
--
-- $setup
-- >>> import Data.Either (isLeft)

-- | Pencil's template for content.
data PNode =
    PText T.Text
  | PVar T.Text
  deriving (Show, Eq)

-- | Data structure that mirrors the tagsoup @Tag@ data. We need more data types for our variables.
data PTag =
  PTagOpen T.Text [TS.Attribute T.Text]
  | PTagClose T.Text
  | PTagText T.Text
  | PTagComment T.Text
  | PTagVariable T.Text
  | PTagWarning T.Text   -- Meta: parse warning
  | PTagPosition Int Int -- Meta: row, col

parsePTags :: T.Text -> [PTag]
parsePTags t = fromTagsoup (TS.parseTags t)

fromTagsoup :: [TS.Tag T.Text] -> [PTag]
fromTagsoup = map fromTagsoup'

fromTagsoup' :: TS.Tag T.Text -> PTag
fromTagsoup' (TS.TagOpen t attrs) = PTagOpen t attrs
fromTagsoup' (TS.TagClose t) = PTagClose t
fromTagsoup' (TS.TagText t) = PTagText t
fromTagsoup' (TS.TagComment t) = PTagComment t
fromTagsoup' (TS.TagWarning t) = PTagWarning t
fromTagsoup' (TS.TagPosition r c) = PTagPosition r c

toTagsoup :: [PTag] -> [TS.Tag T.Text]
toTagsoup = map toTagsoup'

toTagsoup' :: PTag -> TS.Tag T.Text
toTagsoup' (PTagOpen t attrs) = TS.TagOpen t attrs
toTagsoup' (PTagClose t) = TS.TagClose t
toTagsoup' (PTagText t) = TS.TagText t
toTagsoup' (PTagComment t) = TS.TagComment t
toTagsoup' (PTagVariable t) = TS.TagText (T.append "${" (T.append t "}"))
toTagsoup' (PTagWarning _) = TS.TagText ""
toTagsoup' (PTagPosition _ _) = TS.TagText ""

renderNodes :: [PNode] -> T.Text
renderNodes = L.foldl' (\str n -> (T.append str (nodeToDisplay n))) ""

nodeToDisplay :: PNode -> T.Text
nodeToDisplay (PText t) = t
nodeToDisplay (PVar t) = T.append (T.append "${" t) "}"

runParser :: T.Text -> Either ParseError [PNode]
runParser text = parse parseEverything (T.unpack "") (T.unpack text)

-- | Parse everything.
--
-- >>> parse parseEverything "" "Hello ${man} and ${woman}."
-- Right [PText "Hello ",PVar "man",PText " and ",PVar "woman",PText "."]
--
-- >>> parse parseEverything "" "<b>this $$escape works</b> ${realvar}"
-- Right [PText "<b>this ",PText "$$",PText "escape works</b> ",PVar "realvar"]
--
-- | This is a degenerate case that we will just allow (for now) to go sideways:
-- >>> parse parseEverything "" "<b>this ${var never closes</b> ${realvar}"
-- Right [PText "<b>this ",PVar "var never closes</b> ${realvar"]
--
parseEverything :: Parser [PNode]
parseEverything = many1 (parseContent <|> parseEscape <|> parseVar)

-- | Parse variables.
--
-- >>> parse parseVar "" "${ffwe} yep"
-- Right (PVar "ffwe")
--
-- >>> isLeft $ parse parseVar "" "Hello ${name}"
-- True
parseVar :: Parser PNode
parseVar = try $ do
  _ <- char '$'
  _ <- char '{'
  varName <- many (noneOf "}")
  _ <- char '}'
  return $ PVar (T.pack varName)

-- | Parse boring, boring text.
--
-- >>> parse parseContent "" "hello ${ffwe} you!"
-- Right (PText "hello ")
--
-- >>> isLeft $ parse parseContent "" "${name}!!"
-- True
--
-- https://stackoverflow.com/questions/20730488/parsec-read-text-ended-by-a-string
-- https://github.com/jaspervdj/hakyll/blob/32e34f435c7911f36acdf4a62eec1f56faf0b269/src/Hakyll/Web/Template/Internal/Element.hs#L137
parseContent :: Parser PNode
parseContent = do
  -- stuff <- manyTill anyChar (try (string "${"))
  -- stuff <- try (manyTill anyChar parseVar) -- <|> (many1 anyChar)
  stuff <- many1 (noneOf "$")
  -- stuff <- manyTill anyChar (eof <|> lookAhead (parseVar >> return ()))
  return $ PText (T.pack stuff)

parseEscape :: Parser PNode
parseEscape = do
  _ <- try (string "$$")
  return $ PText "$$"

-- | A hack to capture strings that "almost" are templates. I couldn't figure
-- out another way.
parseFakeVar :: Parser PNode
parseFakeVar = do
  _ <- char '$'
  n <- noneOf "{"
  rest <- many1 (noneOf "$")
  return $ PText (T.pack ("$" ++ [n] ++ rest))
