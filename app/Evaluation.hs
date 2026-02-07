{-# LANGUAGE DeriveFunctor #-}

module Evaluation where

import AST
import Data.Map ((!))
import qualified Data.Map as M
import Prelude hiding ((!))

data Ty = TInt | TBool | TChar | TStr | TVec | TList deriving (Eq, Show)

data Val
  = VInt Int
  | VBool Bool
  | VChar Char
  | VStr String
  | VCons Val Val
  | VNull
  | VVec [Val]
  | VVoid
  deriving (Eq, Show)

type Ctx = M.Map Id Val

evalOp0 :: Op0 -> IO Val
evalOp0 ReadByte = VChar <$> getChar

-- | Throws an error if the identifier is not found; consider refactoring to a
evalId :: Ctx -> Id -> Val
evalId ctx id = ctx M.! id

evalOp2 :: Op2 -> Val -> Val -> Val
evalOp2 _ VVoid _ = error "attempted to operate on a void type"
evalOp2 _ _ VVoid = error "attempted to operate on a void type"
evalOp2 Plus (VInt l) (VInt r) = VInt (l + r)
evalOp2 Minus (VInt l) (VInt r) = VInt (l - r)
evalOp2 LessThan (VInt l) (VInt r) = VBool (l < r)
evalOp2 Equals (VInt l) (VInt r) = VBool (l == r)
evalOp2 Equals (VBool l) (VBool r) = VBool (l == r)
evalOp2 Equals (VChar l) (VChar r) = VBool (l == r)
evalOp2 Equals (VStr l) (VStr r) = VBool (l == r)
evalOp2 Equals (VVec l) (VVec r) = VBool (l == r)
evalOp2 Equals _ _ = VBool False
evalOp2 Cons
