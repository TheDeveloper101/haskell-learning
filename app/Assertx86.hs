{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DataKinds #-}

module Assertx86 where
import CodeGen.X86
import CodeGen.X86.Asm
import Types
import Data.Int

assertType :: Label -> Int64 -> Int64 -> Reg S64 -> Code
assertType err mask ty register = mdo
    push $ RegOp register
    and_ (RegOp register) $ ImmOp $ Immediate $ mask
    cmp (RegOp register) $ ImmOp $ Immediate $ ty
    pop $ RegOp register
    j NE err

assertInt :: Label -> Reg S64 -> Code
assertInt err = assertType err maskInt typeInt
assertChar err = assertType err maskChar typeChar
assertBox err = assertType err (fromIntegral ptrMask) $ fromIntegral typeBox
assertCons err = assertType err (fromIntegral ptrMask) $ fromIntegral typeCons
assertVector err = assertType err (fromIntegral ptrMask) $ fromIntegral typeVect
assertString err = assertType err (fromIntegral ptrMask) $ fromIntegral typeStr