{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module CodegenWasm (compileWasm) where

import AST (Expr (..))
import Language.Wasm.Structure (ValueType(..), Expression)
import Language.Wasm.Builder
import Language.Wasm.Interpreter
import Data.Proxy
import Types(valueToBits, bitsToValue)


runWasm :: Expr -> IO ()
runWasm code = do let address = getAddress code
                  invoke emptyStore (error "what address") []
                  pure ()

getAddress :: GenFun (Proxy I64) -> Int
getAddress = error "get me the address pls"

compileWasm :: Expr -> GenFun (Proxy I64)
compileWasm e = case e of
    Int i -> i64c (valueToBits (Int i))
    Bool b -> i64c (valueToBits (Bool b))
    Char c -> i64c (valueToBits (Char c))
    _ -> trap Proxy

