{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Evaluation where

import AST
import Data.Map ((!))
import qualified Data.Map as M
import Prelude hiding ((!))
import Control.Monad.Trans.State

data Ty = TInt | TBool | TChar | TStr | TVec | TList | TBox deriving (Eq, Show)

data Val
  = VInt Int
  | VBool Bool
  | VChar Char
  | VStr String
  | VCons Val Val
  | VNull
  | VVec Int [Val]
  | VBox Int Val
  | VVoid
  deriving (Eq, Show)

type Ctx = M.Map Id Val

evalOp0 :: Op0 -> IO Val
evalOp0 ReadByte = VChar <$> getChar

-- | Throws an error if the identifier is not found; consider refactoring to a
evalId :: Ctx -> Id -> Val
evalId ctx id = ctx M.! id

typeof :: Val -> Ty
typeof (VInt _) = TInt
typeof (VBool _) = TBool
typeof (VChar _) = TChar
typeof (VStr _) = TStr
typeof (VCons _ _) = TList
typeof VNull = TList
typeof (VVec _ _) = TVec
typeof (VBox _ _) = TBox
typeof VVoid = error "tried to get the type of a void value"

newtype Incr a = Incr (State Int a) deriving (Functor, Applicative, Monad)

incr :: Incr Int
incr = Incr $ do
  i <- get
  put (i + 1)
  return i

evalIncr :: Incr a -> Int -> a
evalIncr (Incr st) = evalState st

evalOp2 :: Op2 -> Val -> Val -> Incr Val
evalOp2 _ VVoid _ = error "attempted to operate on a void type"
evalOp2 _ _ VVoid = error "attempted to operate on a void type"
evalOp2 Plus (VInt l) (VInt r) = return $ VInt (l + r)
evalOp2 Minus (VInt l) (VInt r) = return $ VInt (l - r)
evalOp2 LessThan (VInt l) (VInt r) = return $ VBool (l < r)
evalOp2 Equals (VInt l) (VInt r) = return $ VBool (l == r)
evalOp2 Equals (VBool l) (VBool r) = return $ VBool (l == r)
evalOp2 Equals (VChar l) (VChar r) = return $ VBool (l == r)
evalOp2 Equals (VStr l) (VStr r) = return $ VBool (l == r)
evalOp2 Equals _ _ = return $ VBool False
evalOp2 Cons x y = return $ VCons x y
evalOp2 EqHuh l r = return $ VBool (l == r)
evalOp2 MakeVector (VInt length) defaultValue =
  do memloc <- incr;
     pure $ VVec memloc (replicate length defaultValue)
evalOp2 VectorRef (VVec _ xs) (VInt idx)
  | 0 < idx && idx < length xs = return $ xs !! idx
  | otherwise = return $ error "attempted out-of-bounds reference"
evalOp2 MakeString (VInt length) (VChar c) = return $ VStr $ replicate length c
evalOp2 StringRef (VStr cs) (VInt idx)
  | 0 < idx && idx < length cs = return $ VChar $ cs !! idx
  | otherwise = return $ error "attempted out-of-bounds reference"
evalOp2 _ _ _ = error "type error"
