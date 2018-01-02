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
  | PFor T.Text -- for variable
  | PEndFor
  | PIf T.Text -- if variable
  | PEndIf
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
nodeToDisplay (PFor t) = T.append (T.append "${for(" t) ")}"
nodeToDisplay (PEndFor) = "${endfor}"
nodeToDisplay (PIf t) = T.append (T.append "${if(" t) ")}"
nodeToDisplay (PEndIf) = "${endif}"

runParser :: T.Text -> Either ParseError [PNode]
runParser text = parse parseEverything (T.unpack "") (T.unpack text)

-- | Parse everything.
--
-- >>> parse parseEverything "" "Hello ${man} and ${woman}."
-- Right [PText "Hello ",PVar "man",PText " and ",PVar "woman",PText "."]
--
-- >>> parse parseEverything "" "Hello ${man} and ${if(woman)} text here ${endif}."
-- Right [PText "Hello ",PVar "man",PText " and ",PIf "woman",PText " text here ",PEndIf,PText "."]
--
-- >>> parse parseEverything "" "Hi ${for(people)} ${name}, ${endfor} everyone!"
-- Right [PText "Hi ",PFor "people",PText " ",PVar "name",PText ", ",PEndFor,PText " everyone!"]
--
-- >>> parse parseEverything "" "<b>this $$escape works</b> ${realvar}"
-- Right [PText "<b>this ",PText "$",PText "escape works</b> ",PVar "realvar"]
--
-- | This is a degenerate case that we will just allow (for now) to go sideways:
-- >>> parse parseEverything "" "<b>this ${var never closes</b> ${realvar}"
-- Right [PText "<b>this ",PVar "var never closes</b> ${realvar"]
--
parseEverything :: Parser [PNode]
parseEverything =
  -- Note that order matters here. We want "most general" to be last (variable
  -- names).
  many1 (parseContent
     <|> try parseEscape
     <|> try parseEndFor
     <|> try parseFor
     <|> try parseEndIf
     <|> try parseIf
     <|> parseVar)

-- | Parse variables.
--
-- >>> parse parseVar "" "${ffwe} yep"
-- Right (PVar "ffwe")
--
-- >>> parse parseVar "" "${spaces technically allowed}"
-- Right (PVar "spaces technically allowed")
--
-- >>> isLeft $ parse parseVar "" "Hello ${name}"
-- True
--
-- >>> isLeft $ parse parseVar "" "${}"
-- True
--
parseVar :: Parser PNode
parseVar = try $ do
  _ <- char '$'
  _ <- char '{'
  varName <- many1 (noneOf "}")
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

-- | Parse "$$" to escape as "$".
--
-- >>> parse parseEscape "" "$$"
-- Right (PText "$")
--
-- >>> isLeft $ parse parseEscape "" "$"
-- True
--
-- >>> parse parseEscape "" "$$$"
-- Right (PText "$")
parseEscape :: Parser PNode
parseEscape = do
  _ <- try (string "$$")
  return $ PText "$"

-- | Parse for loop declaration.
--
-- >>> parse parseFor "" "${for(posts)}"
-- Right (PFor "posts")
--
-- >>> parse parseFor "" "${for(variable name with spaces technically allowed)}"
-- Right (PFor "variable name with spaces technically allowed")
--
-- >>> isLeft $ parse parseFor "" "${for()}"
-- True
--
-- >>> isLeft $ parse parseFor "" "${for foo}"
-- True
--
parseFor :: Parser PNode
parseFor = parseFunction "for" PFor

parseIf :: Parser PNode
parseIf = parseFunction "if" PIf

parseFunction :: String -> (T.Text -> PNode) -> Parser PNode
parseFunction keyword ctor = do
  _ <- char '$'
  _ <- char '{'
  _ <- try $ string (keyword ++ "(")
  varName <- many1 (noneOf ")")
  _ <- char ')'
  _ <- char '}'
  return $ ctor (T.pack varName)

-- | Parse endfor keyword.
--
-- >>> parse parseEndFor "" "${endfor}"
-- Right PEndFor
--
-- >>> isLeft $ parse parseEndFor "" "${endforrr}"
-- True
--
parseEndFor :: Parser PNode
parseEndFor = parseEnd "endfor" PEndFor

parseEndIf :: Parser PNode
parseEndIf = parseEnd "endif" PEndIf

parseEnd :: String -> PNode -> Parser PNode
parseEnd keyword ctor = do
  _ <- char '$'
  _ <- char '{'
  _ <- try $ string keyword
  _ <- char '}'
  return ctor

-- | A hack to capture strings that "almost" are templates. I couldn't figure
-- out another way.
parseFakeVar :: Parser PNode
parseFakeVar = do
  _ <- char '$'
  n <- noneOf "{"
  rest <- many1 (noneOf "$")
  return $ PText (T.pack ("$" ++ [n] ++ rest))
