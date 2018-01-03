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

-- | Pencil's AST.
data PNode =
    PText T.Text
  | PVar T.Text
  | PFor T.Text [PNode]
  | PIf T.Text [PNode]

  -- Signals a If/For expression in the stack waiting for expressions. So that we
  -- can find the next unused open if/for-statement in nested if/for-statements.
  | PMetaIf T.Text
  | PMetaFor T.Text

  -- A terminating node that represents the end of the program, to help with AST
  -- converstion
  | PMetaEnd
  deriving (Show, Eq)

-- | Pencil's tokens for content.
data Token =
    TokText T.Text
  | TokVar T.Text
  | TokFor T.Text -- for variable
  | TokIf T.Text -- if variable
  | TokEnd
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

-- | Convert Tokens to PNode AST.o
--
-- >>> transform [TokText "hello", TokText "world"]
-- ([PText "hello",PText "world"],[])
--
-- >>> transform [TokIf "title", TokEnd]
-- ([PIf "title" []],[])
--
-- >>> transform [TokIf "title", TokText "hello", TokText "world", TokEnd]
-- ([PIf "title" [PText "hello",PText "world"]],[])
--
-- ${if(title)}
--   ${for(posts)}
--     world
--   ${end}
-- ${end}
--
-- >>> transform [TokIf "title", TokFor "posts", TokText "world", TokEnd, TokEnd]
-- ([PIf "title" [PFor "posts" [PText "world"]]],[])
--
-- begin
-- now
-- ${if(title)}
--   hello
--   world
--   ${if(body)}
--     ${body}
--     ${someothervar}
--     wahh
--   ${end}
--   final
--   thing
-- ${end}
-- the
-- lastline
--
-- >>> transform [TokText "begin", TokText "now", TokIf "title", TokText "hello", TokText "world", TokIf "body", TokVar "body", TokVar "someothervar", TokText "wahh", TokEnd, TokText "final", TokText "thing", TokEnd, TokText "the", TokText "lastline"]
-- ([PText "begin",PText "now",PIf "title" [PText "hello",PText "world",PIf "body" [PVar "body",PVar "someothervar",PText "wahh"],PText "final",PText "thing"],PText "the",PText "lastline"],[])
--
transform :: [Token] -> ([PNode], [Token])
transform toks =
  let (stack, leftovers) = ast [] toks
  in (reverse stack, leftovers)

ast :: [PNode] -- stack
    -> [Token] -- remaining
    -> ([PNode], [Token]) -- (AST, remaining)
ast stack [] = (stack, [])
ast stack (TokText t : toks) = ast (PText t : stack) toks
ast stack (TokVar t : toks)  = ast (PVar t : stack) toks
ast stack (TokIf t : toks)   = ast (PMetaIf t : stack) toks
ast stack (TokFor t : toks)  = ast (PMetaFor t : stack) toks
ast stack (TokEnd : toks) =
  let (node, popped, remaining) = popNodes stack
      -- ^ Find the last unused if/for statement, and grab all the expressions
      -- in-between this TokEnd and the opening if/for keyword.
      n = case node of
            PMetaIf t -> PIf t popped
            PMetaFor t -> PFor t popped
            _ -> PMetaEnd
  -- Push the statement into the stack
  in ast (n : remaining) toks

-- | Pop nodes until we hit a If/For statement.
-- Return pair (constructor found, nodes popped, remaining stack)
popNodes :: [PNode] -> (PNode, [PNode], [PNode])
popNodes = popNodes' []

popNodes' :: [PNode] -> [PNode] -> (PNode, [PNode], [PNode])
popNodes' popped [] = (PMetaEnd, popped, [])
popNodes' popped (PMetaIf t : rest) = (PMetaIf t, popped, rest)
popNodes' popped (PMetaFor t : rest) = (PMetaFor t, popped, rest)
popNodes' popped (t : rest) = popNodes' (t : popped) rest

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
renderNodes = L.foldl' (\str n -> (T.append str (renderNode n))) ""

renderNode :: PNode -> T.Text
renderNode (PText t) = t
renderNode (PVar t) = T.append (T.append "${" t) "}"
renderNode (PFor t nodes) =
  let for = T.append (T.append "${for(" t) ")}"
      body = renderNodes nodes
      end = "${end}"
  in T.append (T.append for body) end
renderNode (PIf t nodes) =
  let for = T.append (T.append "${if(" t) ")}"
      body = renderNodes nodes
      end = "${end}"
  in T.append (T.append for body) end

renderTokens :: [Token] -> T.Text
renderTokens = L.foldl' (\str n -> (T.append str (renderToken n))) ""

renderToken :: Token -> T.Text
renderToken (TokText t) = t
renderToken (TokVar t) = T.append (T.append "${" t) "}"
renderToken (TokFor t) = T.append (T.append "${for(" t) ")}"
renderToken (TokEnd) = "${end}"
renderToken (TokIf t) = T.append (T.append "${if(" t) ")}"

runParser :: T.Text -> Either ParseError [PNode]
runParser text = do
  toks <- parse parseEverything (T.unpack "") (T.unpack text)
  return $ fst $ transform toks

-- | Parse everything.
--
-- >>> parse parseEverything "" "Hello ${man} and ${woman}."
-- Right [TokText "Hello ",TokVar "man",TokText " and ",TokVar "woman",TokText "."]
--
-- >>> parse parseEverything "" "Hello ${man} and ${if(woman)} text here ${endif}."
-- Right [TokText "Hello ",TokVar "man",TokText " and ",TokIf "woman",TokText " text here ",TokEnd,TokText "."]
--
-- >>> parse parseEverything "" "Hi ${for(people)} ${name}, ${end} everyone!"
-- Right [TokText "Hi ",TokFor "people",TokText " ",TokVar "name",TokText ", ",TokEnd,TokText " everyone!"]
--
-- >>> parse parseEverything "" "<b>this $$escape works</b> ${realvar}"
-- Right [TokText "<b>this ",TokText "$",TokText "escape works</b> ",TokVar "realvar"]
--
-- | This is a degenerate case that we will just allow (for now) to go sideways:
-- >>> parse parseEverything "" "<b>this ${var never closes</b> ${realvar}"
-- Right [TokText "<b>this ",TokVar "var never closes</b> ${realvar"]
--
parseEverything :: Parser [Token]
parseEverything =
  -- Note that order matters here. We want "most general" to be last (variable
  -- names).
  many1 (parseContent
     <|> try parseEscape
     <|> try parseEnd
     <|> try parseFor
     <|> try parseEnd
     <|> try parseIf
     <|> parseVar)

-- | Parse variables.
--
-- >>> parse parseVar "" "${ffwe} yep"
-- Right (TokVar "ffwe")
--
-- >>> parse parseVar "" "${spaces technically allowed}"
-- Right (TokVar "spaces technically allowed")
--
-- >>> isLeft $ parse parseVar "" "Hello ${name}"
-- True
--
-- >>> isLeft $ parse parseVar "" "${}"
-- True
--
parseVar :: Parser Token
parseVar = try $ do
  _ <- char '$'
  _ <- char '{'
  varName <- many1 (noneOf "}")
  _ <- char '}'
  return $ TokVar (T.pack varName)

-- | Parse boring, boring text.
--
-- >>> parse parseContent "" "hello ${ffwe} you!"
-- Right (TokText "hello ")
--
-- >>> isLeft $ parse parseContent "" "${name}!!"
-- True
--
-- https://stackoverflow.com/questions/20730488/parsec-read-text-ended-by-a-string
-- https://github.com/jaspervdj/hakyll/blob/32e34f435c7911f36acdf4a62eec1f56faf0b269/src/Hakyll/Web/Template/Internal/Element.hs#L137
parseContent :: Parser Token
parseContent = do
  -- stuff <- manyTill anyChar (try (string "${"))
  -- stuff <- try (manyTill anyChar parseVar) -- <|> (many1 anyChar)
  stuff <- many1 (noneOf "$")
  -- stuff <- manyTill anyChar (eof <|> lookAhead (parseVar >> return ()))
  return $ TokText (T.pack stuff)

-- | Parse "$$" to escape as "$".
--
-- >>> parse parseEscape "" "$$"
-- Right (TokText "$")
--
-- >>> isLeft $ parse parseEscape "" "$"
-- True
--
-- >>> parse parseEscape "" "$$$"
-- Right (TokText "$")
parseEscape :: Parser Token
parseEscape = do
  _ <- try (string "$$")
  return $ TokText "$"

-- | Parse for loop declaration.
--
-- >>> parse parseFor "" "${for(posts)}"
-- Right (TokFor "posts")
--
-- >>> parse parseFor "" "${for(variable name with spaces technically allowed)}"
-- Right (TokFor "variable name with spaces technically allowed")
--
-- >>> isLeft $ parse parseFor "" "${for()}"
-- True
--
-- >>> isLeft $ parse parseFor "" "${for foo}"
-- True
--
parseFor :: Parser Token
parseFor = parseFunction "for" TokFor

parseIf :: Parser Token
parseIf = parseFunction "if" TokIf

parseFunction :: String -> (T.Text -> Token) -> Parser Token
parseFunction keyword ctor = do
  _ <- char '$'
  _ <- char '{'
  _ <- try $ string (keyword ++ "(")
  varName <- many1 (noneOf ")")
  _ <- char ')'
  _ <- char '}'
  return $ ctor (T.pack varName)

-- | Parse end keyword.
--
-- >>> parse parseEnd "" "${end}"
-- Right TokEnd
--
-- >>> isLeft $ parse parseEnd "" "${enddd}"
-- True
--
parseEnd :: Parser Token
parseEnd = do
  _ <- try $ string "${end}"
  return TokEnd

-- | A hack to capture strings that "almost" are templates. I couldn't figure
-- out another way.
parseFakeVar :: Parser Token
parseFakeVar = do
  _ <- char '$'
  n <- noneOf "{"
  rest <- many1 (noneOf "$")
  return $ TokText (T.pack ("$" ++ [n] ++ rest))
