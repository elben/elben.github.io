module Pencil.Parser where

import Text.ParserCombinators.Parsec
import qualified Data.Text as T

-- Doctest setup.
--
-- $setup
-- >>> import Data.Either (isLeft)

data SimpleTemplate =
    STText T.Text
  | STVar T.Text
  deriving (Show, Eq)

-- | Parse everything.
--
-- >>> parse parseEverything "" "Hello ${man} and ${woman}."
-- Right [STText "Hello ",STVar "man",STText " and ",STVar "woman",STText "."]
--
parseEverything :: Parser [SimpleTemplate]
parseEverything = many1 (parseContent <|> parseVar)

-- | Parse variables.
--
-- >>> parse parseVar "" "${ffwe} yep"
-- Right (STVar "ffwe")
--
-- >>> isLeft $ parse parseVar "" "Hello ${name}"
-- True
parseVar :: Parser SimpleTemplate
parseVar = do
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
parseContent :: Parser SimpleTemplate
parseContent = do
  stuff <- many1 (noneOf "$")
  return $ STText (T.pack stuff)

