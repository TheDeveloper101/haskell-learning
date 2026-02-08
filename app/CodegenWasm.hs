{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module CodegenWasm (compileWasm) where

import AST (Expr (..))
import Language.Wasm.Structure (ValueType(..))
import Language.Wasm.Builder
import Data.Proxy
import Types(valueToBits, bitsToValue)

compileWasm :: Expr -> GenFun (Proxy I64)

compileWasm e = case e of
    Int i -> i64c (valueToBits i)
    _ -> trap Proxy