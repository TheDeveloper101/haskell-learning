{-# LANGUAGE DeriveFunctor #-}
module Evaluation where

import qualified Data.Map as M
import AST

data Val
  = VInt  Int
  | VBool Bool
  | VChar Char
  | VStr  String
  deriving (Eq, Show)

data Ctx a = Ctx (M.Map Id Val) a deriving (Eq, Show, Functor)


evalOp0 :: Op0 -> IO Val
evalOp0 ReadByte = VChar <$> getChar

-- | Throws an error if the identifier is not found; consider refactoring to a 
evalId :: Ctx Id -> Val
evalId = undefined
