module Parser where

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

parseEmpty :: Parser Expr
parseEmpty = Empty <$ string "'()"

-- todo: other specially printed racket chars
parseChar :: Parser Expr
parseChar = Char <$> (('\n' <$ string "#\\newline")
                 <|> (' ' <$ string "#\\space")
                 <|> (string "#\\" *> printChar))

parseString :: Parser Expr
parseString = Str <$> (char '"' *> many (anySingleBut '"') <* char '"')

