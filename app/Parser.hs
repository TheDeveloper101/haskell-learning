module Parser where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

import AST

type Parser = Parsec Void String

parseBool :: Parser Expr
parseBool = Bool False <$ string "#f" <|> Bool True <$ string "#t"
