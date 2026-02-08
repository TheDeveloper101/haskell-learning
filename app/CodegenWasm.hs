{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CodegenWasm (runWasm) where

import AST (Expr (..))
import Language.Wasm.Structure (ValueType(..), Module)
import Language.Wasm.Builder
import Data.Proxy
import Types(valueToBits, bitsToValue)
import Control.Monad(void)
import Language.Wasm (validate)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Language.Wasm.Interpreter (emptyStore, instantiate, invokeExport, Value (VI64))
import Unsafe.Coerce (unsafeCoerce)

extractResult :: Value -> Expr
extractResult (VI64 v) = bitsToValue (unsafeCoerce v)

runWasm :: Expr -> IO (Maybe [Expr])
runWasm = (fmap . fmap . fmap) extractResult . runWasmInt . createModule . compileWasm 

runWasmInt :: Module -> IO (Maybe [Value])
runWasmInt mod =
    do  let validModule = fromRight (error "failed validation") . validate $ mod
        (maybeModInstance, store) <- instantiate emptyStore Map.empty validModule
        let modInstance = fromRight (error "failed instantiate") maybeModInstance
        invokeExport store modInstance "test" []

createModule :: GenFun (Proxy I64) -> Module
createModule = genMod . export "test" . fun Proxy

compileWasm :: Expr -> GenFun (Proxy I64)
compileWasm e = case e of
    Int i -> i64c (valueToBits (Int i))
    Bool b -> i64c (valueToBits (Bool b))
    Char c -> i64c (valueToBits (Char c))
    _ -> trap Proxy

