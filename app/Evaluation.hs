{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Evaluation where

import AST
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict (StateT, put, get, evalStateT)
import Control.Monad.Trans.Class (lift, MonadTrans)
import Data.Maybe (fromMaybe)

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
  | VEoF
  deriving (Eq, Show)

type Ctx = M.Map Id Val

evalOp0 :: Op0 -> Val
evalOp0 ReadByte = VInt (ord <$> getChar)

evalOp1 :: Op1 -> Val -> Val
evalOp1 Add1 (VInt v) = VInt (v + 1)
evalOp1 Sub1 (VInt v) = VInt (v - 1)
evalOp1 ZeroHuh (VInt v) = VBool (v == 0)
--evalOp1 IntegerToChar (VInt v) = VChar (read v)
--evalOp1 CharToInteger (VChar c) = VInt (read c)
--evalOp1 WriteByte (VInt v) = chr v <$> getChar
evalOp1 EofObjectHuh v = VBool (v == VEoF)
evalOp1 Box val = VBox val
evalOp1 Car (VVec _ v) = head v
evalOp1 Cdr (VVec _ v) = VVec (tail v)
evalOp1 Unbox (VBox _ v) = v
evalOp1 EmptyHuh v = VBool (v == VNull)
evalOp1 ConsHuh (VCons _ _) = VBool True
evalOp1 ConsHuh _ = VBool False
evalOp1 BoxHuh (VBox _ _) = VBool True
evalOp1 BoxHuh _ = VBool False
evalOp1 VectorHuh (VVec _ _) = VBool True
evalOp1 VectorHuh _ = VBool False
evalOp1 VectorLength (VVec _ v) = VInt (length v)
evalOp1 _ _ = error "todo"
-- evalOp1 CharHuh expr = 
-- evalOp1 IntegerToChar expr 

-- | Throws an error if the identifier is not found; consider refactoring to a
evalId :: Ctx -> Id -> Val
evalId ctx id = ctx M.! id

evalOp1 :: Op1 -> Val -> IncrT IO Val
evalOp1 = undefined

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

newtype IncrT m a = IncrT (StateT Int m a) deriving (Functor,
                                                     Applicative,
                                                     Monad,
                                                     MonadTrans)

incr :: Monad m => IncrT m Int
incr = IncrT $ do
  i <- get
  put (i + 1)
  return i

evalIncrT :: Monad m => IncrT m a -> Int -> m a
evalIncrT (IncrT st) = evalStateT st

evalOp2 :: Monad m => Op2 -> Val -> Val -> IncrT m Val
evalOp2 _ VVoid _ = error "attempted to operate on a void type"
evalOp2 _ _ VVoid = error "attempted to operate on a void type"
evalOp2 Plus (VInt l) (VInt r) = pure $ VInt (l + r)
evalOp2 Minus (VInt l) (VInt r) = pure $ VInt (l - r)
evalOp2 LessThan (VInt l) (VInt r) = pure $ VBool (l < r)
evalOp2 Equals (VInt l) (VInt r) = pure $ VBool (l == r)
evalOp2 Equals (VBool l) (VBool r) = pure $ VBool (l == r)
evalOp2 Equals (VChar l) (VChar r) = pure $ VBool (l == r)
evalOp2 Equals (VStr l) (VStr r) = pure $ VBool (l == r)
evalOp2 Equals _ _ = pure $ VBool False
evalOp2 Cons x y = pure $ VCons x y
evalOp2 EqHuh l r = pure $ VBool (l == r)
evalOp2 MakeVector (VInt length) defaultValue =
  do memloc <- incr
     pure $ VVec memloc (replicate length defaultValue)
evalOp2 VectorRef (VVec _ xs) (VInt idx)
  | 0 < idx && idx < length xs = pure $ xs !! idx
  | otherwise = pure $ error "attempted out-of-bounds reference"
evalOp2 MakeString (VInt length) (VChar c) = pure $ VStr $ replicate length c
evalOp2 StringRef (VStr cs) (VInt idx)
  | 0 < idx && idx < length cs = pure $ VChar $ cs !! idx
  | otherwise = pure $ error "attempted out-of-bounds reference"
evalOp2 _ _ _ = error "type error"

evalOp3 :: Op3 -> Val -> Val -> Val -> IncrT m Val
evalOp3 _ _ _ = undefined

isTruthy :: Val -> Bool
isTruthy (VBool True) = True
isTruthy _ = False

eval' :: Ctx -> Expr -> IncrT IO (Ctx, Val)
eval' ctx Empty          = pure (ctx, VVoid)
eval' ctx Eof            = pure (ctx, VVoid)
eval' ctx (Int n)        = pure (ctx, VInt n)
eval' ctx (Bool b)       = pure (ctx, VBool b)
eval' ctx (Char c)       = pure (ctx, VChar c)
eval' ctx (Str s)        = pure (ctx, VStr s)
eval' ctx (Prim0 op)     = (ctx,) <$> lift (evalOp0 op)
eval' ctx (Prim1 op arg) = do (_, varg) <- eval' ctx arg
                              (ctx,) <$> evalOp1 op varg
eval' ctx (Prim2 op l r) = do (_, vl) <- eval' ctx l
                              (_, vr) <- eval' ctx r
                              (ctx,) <$> evalOp2 op vl vr
eval' ctx (Prim3 op a1 a2 a3) = do (_, va1) <- eval' ctx a1
                                   (_, va2) <- eval' ctx a2
                                   (_, va3) <- eval' ctx a3
                                   (ctx,) <$> evalOp3 op va1 va2 va3
eval' ctx (If p t f)     = do (_, vp) <- eval' ctx p
                              (_, vt) <- eval' ctx t
                              (_, vf) <- eval' ctx f
                              pure (ctx, if isTruthy vp then vt else vf)
eval' ctx (Begin l r)    = do (ctx', _) <- eval' ctx l
                              eval' ctx' r
eval' ctx (Let binder rexp body) = do (_, vrexp) <- eval' ctx rexp
                                      let ctx' = M.insert binder vrexp ctx
                                      eval' ctx' body
eval' ctx (Var id) = pure (ctx, fromMaybe (error "undefined variable")
                                 $ M.lookup id ctx)

eval :: Expr -> IO Val
eval = flip evalIncrT 0 . fmap snd . eval' M.empty
