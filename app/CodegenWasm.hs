{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module CodegenWasm where

import AST
import Language.Wasm.Structure (ValueType(..), Module)
import Language.Wasm.Builder
import Data.Proxy
import TypesWasm(valueToBits, bitsToValue)
import Language.Wasm (validate)
import Data.Either (fromRight)
import qualified Data.Map as Map
import Language.Wasm.Interpreter (emptyStore, instantiate, invokeExport, Value (VI64))
import Unsafe.Coerce (unsafeCoerce)

extractResult :: Value -> Expr
extractResult (VI64 v) = bitsToValue (unsafeCoerce v)

runWasm :: Expr -> IO (Maybe [Expr])
runWasm = (fmap . fmap . fmap) extractResult . runWasmInt . createModule . compileWasm 

runWasmInt' :: Expr -> IO (Maybe [Value])
runWasmInt' = runWasmInt . createModule . compileWasm 

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
    Prim1 op1 e -> compileOp1Wasm op1 (compileWasm e) 
    Prim2 op2 e1 e2 -> compileOp2Wasm op2 (compileWasm e1) (compileWasm e2)
    _ -> trap Proxy

compileOp1Wasm :: Op1 -> GenFun (Proxy I64) -> GenFun (Proxy I64)
compileOp1Wasm op1 gf = case op1 of
    Add1 -> add (i64c (valueToBits (Int 1))) gf
    Sub1 -> sub gf (i64c (valueToBits (Int 1)))
    ZeroHuh -> add (shl (extend_u (eqz gf)) (i64c 5)) (i64c 24) 
    _ -> trap Proxy

compileOp2Wasm :: Op2 -> GenFun (Proxy I64) -> GenFun (Proxy I64) -> GenFun (Proxy I64)
compileOp2Wasm op2 gf1 gf2 = case op2 of
    Plus -> add gf1 gf2
    Minus -> sub gf1 gf2
    _ -> trap Proxy