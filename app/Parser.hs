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

parseChar :: Parser Expr
parseChar = Char <$> (char '\'' *> printChar <* char '\'')
