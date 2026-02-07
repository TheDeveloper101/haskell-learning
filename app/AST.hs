module AST ( Expr (..), Id, Op0 (..), Op1 (..), Op2 (..), Op3 (..) ) where

data Expr = Eof
          | Empty
          | Int Int
          | Bool Bool
          | Char Char
          | Str String
          | Prim0 Op0
          | Prim1 Op1 Expr
          | Prim2 Op2 Expr Expr
          | Prim3 Op3 Expr Expr Expr
          | If Expr Expr Expr
          | Begin Expr Expr
          | Let Id Expr Expr
          | Var Id
type Id   = String
data Op0  = ReadByte
data Op1  = Add1 | Sub1 | ZeroHuh
          | CharHuh | IntegerToChar | CharToInteger
          | WriteByte | EofObjectHuh
          | Box | Car | Cdr | Unbox
          | EmptyHuh | ConsHuh | BoxHuh
          | VectorHuh | VectorLength
          | StringHuh | StringLength
data Op2  = Plus | Minus | LessThan | Equals
          | Cons | EqHuh
          | MakeVector | VectorRef
          | MakeString | StringRef
data Op3  = VectorSetBang
