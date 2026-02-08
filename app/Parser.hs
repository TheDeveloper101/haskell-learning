module Parser (module Parser, Text.Megaparsec.parseTest) where

import Control.Monad
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import AST

type Parser = Parsec Void String

skipSpace :: Parser ()
skipSpace = L.space
  space1
  (L.skipLineComment ";;")
  (L.skipBlockCommentNested "(*" "*)")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parseId :: Parser Id
parseId = lexeme $ some $ noneOf "()[]{}\",'`;#|\\ "

parseEof :: Parser Expr
parseEof = Eof <$ string "eof"

parseBool :: Parser Expr
parseBool = Bool False <$ string "#f" <|> Bool True <$ string "#t"

parseInt :: Parser Expr
parseInt = Int . read <$> (parseBare <|> parsePos <|> parseNeg)
  where
    parseBare = some numberChar
    parsePos = char '+' *> some numberChar
    parseNeg = (:) <$> char '-' <*> some numberChar

parseEmpty :: Parser Expr
parseEmpty = Empty <$ (string "'(" >> space1 >> string ")")

-- based off racket reference 1.3.14
parseChar :: Parser Expr
parseChar = Char <$>
  (string "#\\" *> (((
      ('\0' <$ (string "null" <|> string "nul"))
  <|> ('\b' <$ string "backspace")
  <|> ('\t' <$ string "tab")
  <|> ('\n' <$ (string "newline" <|> string "linefeed"))
  <|> ('\v' <$ string "vtab")
  <|> ('\f' <$ string "page")
  <|> ('\r' <$ string "return")
  <|> (' ' <$ string "space")
  <|> ('\DEL' <$ string "rubout")
  ) <* notFollowedBy letterChar)
  -- TODO: missing #\octal, #\u, and #\U modes
  <|> printChar))

parseString :: Parser Expr
parseString = Str <$> (char '"' *> many (parseEscapeChar <|> noneOf "\\\"") <* char '"')
  where
    -- based off racket reference 1.3.7
    parseEscapeChar = do
      _ <- char '\\'
      x <- oneOf "abtnvfre\"'\\"
      return $ case x of
        'a' -> '\a'
        'b' -> '\b'
        't' -> '\t'
        'n' -> '\n'
        'v' -> '\v'
        'f' -> '\f'
        'r' -> '\r'
        'e' -> '\ESC'
        _ -> x
    -- TODO: missing \octal, \x, \u, \u\u and \U modes
    -- TODO: handle elided newlines

parsePrimN :: Parser Expr
parsePrimN = do
  void $ lexeme (char '(')
  i <- parseId
  xs <- many parseRecursive
  void $ char ')'
  case (xs, i) of
    ([], "read-byte") -> return $ Prim0 ReadByte
    ([], "peek-byte") -> return $ Prim0 ReadByte
    ([], "void")      -> return $ Prim0 ReadByte

    ([x], "add1")          -> return $ Prim1 Add1 x
    ([x], "sub1")          -> return $ Prim1 Sub1 x
    ([x], "zero?")         -> return $ Prim1 ZeroHuh x
    ([x], "char?")         -> return $ Prim1 CharHuh x
    ([x], "integer->char") -> return $ Prim1 IntegerToChar x
    ([x], "char->integer") -> return $ Prim1 CharToInteger x
    ([x], "write-byte")    -> return $ Prim1 WriteByte x
    ([x], "eof-object?")   -> return $ Prim1 EofObjectHuh x
    ([x], "box")           -> return $ Prim1 Box x
    ([x], "car")           -> return $ Prim1 Car x
    ([x], "cdr")           -> return $ Prim1 Cdr x
    ([x], "unbox")         -> return $ Prim1 Unbox x
    ([x], "empty?")        -> return $ Prim1 EmptyHuh x
    ([x], "cons?")         -> return $ Prim1 ConsHuh x
    ([x], "box?")          -> return $ Prim1 BoxHuh x
    ([x], "vector?")       -> return $ Prim1 VectorHuh x
    ([x], "vector-length") -> return $ Prim1 VectorLength x
    ([x], "string?")       -> return $ Prim1 StringHuh x
    ([x], "string-length") -> return $ Prim1 StringLength x

    ([x, y], "+")           -> return $ Prim2 Plus x y
    ([x, y], "-")           -> return $ Prim2 Minus x y
    ([x, y], "<")           -> return $ Prim2 LessThan x y
    ([x, y], "=")           -> return $ Prim2 Equals x y
    ([x, y], "cons")        -> return $ Prim2 Cons x y
    ([x, y], "eq?")         -> return $ Prim2 EqHuh x y
    ([x, y], "make-vector") -> return $ Prim2 MakeVector x y
    ([x, y], "vector-ref")  -> return $ Prim2 VectorRef x y
    ([x, y], "make-string") -> return $ Prim2 MakeString x y
    ([x, y], "string-ref")  -> return $ Prim2 StringRef x y

    ([x, y, z], "vector-set!") -> return $ Prim3 VectorSetBang x y z

    _ -> fail "Invalid primN operation"

parseRecursive :: Parser Expr
parseRecursive = lexeme $ parseEof <|> parseEmpty <|> parseInt <|> parseBool
        <|> parseChar <|> parseString <|> parsePrimN

parseExpr :: String -> Either String Expr
parseExpr input =
  case parse (skipSpace *> parseRecursive <* eof) "" input of
    Left err -> Left $ errorBundlePretty err
    Right out -> Right out
