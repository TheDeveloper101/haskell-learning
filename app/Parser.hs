module Parser (module Parser, Text.Megaparsec.parseTest) where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import AST

type Parser = Parsec Void String

parseBool :: Parser Expr
parseBool = Bool False <$ string "#f" <|> Bool True <$ string "#t"

parseIntPos :: Parser Expr
parseIntPos = Int . read <$> some numberChar

parseIntNeg :: Parser Expr
parseIntNeg = Int . read <$> ((:) <$> char '-' <*> some numberChar)

-- todo: specially printed racket chars e.g. #\space and #\newline
parseChar :: Parser Expr
parseChar = Char <$> (string "#\\" *> printChar)

parseString :: Parser Expr
parseString = Str <$> (char '"' *> many (anySingleBut '"') <* char '"')

