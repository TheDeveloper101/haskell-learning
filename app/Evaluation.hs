{-# LANGUAGE DeriveFunctor #-}
module Evaluation where

import Prelude hiding ((!))
import qualified Data.Map as M
import Data.Map ((!))
import AST

data Ty = TInt | TBool | TChar | TStr | TVec deriving (Eq, Show)

data Val
  = VInt  Int
  | VBool Bool
  | VChar Char
  | VStr  String
  | VVec  [Val]
  deriving (Eq, Show)

type Ctx = M.Map Id Val

evalOp0 :: Op0 -> IO Val
evalOp0 ReadByte = VChar <$> getChar

-- | Throws an error if the identifier is not found; consider refactoring to a 
evalId :: Ctx -> Id -> Val
evalId ctx id = ctx M.! id

evalOp2 :: Ctx -> Op2 -> Expr -> Expr -> Val
evalOp2 ctx op l r = undefined
