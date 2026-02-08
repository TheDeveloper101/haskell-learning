{-# LANGUAGE RecursiveDo #-}

module Codegen where

import AST
import CodeGen.X86
import CodeGen.X86.Asm
import Data.Char (ord)
import Data.Int
import System.Process (callCommand)
import Types (bitsToValue, valueToBits)

type Ctx = Integer

build :: Code -> IO ()
build c = do
  writeFile "racket-test.o" (show c)
  callCommand "ld racket-test.o"

wrapBool :: Code
wrapBool = do
  mov rax $ ImmOp $ Immediate $ valueToBits $ Bool True
  elsel <- label
  mov rax $ ImmOp $ Immediate 6
  nop


pushCallerSaved :: Code
pushCallerSaved = do
  push rax
  push rcx
  push rdx
  push rsi
  push r8
  push r9
  push r11

popCallerSaved :: Code
popCallerSaved = do
  pop r11
  pop r9
  pop r8
  pop rsi
  pop rdx
  pop rcx
  pop rax

-- Arg in rdi
-- Puts result in rax as usual
malloc :: Code
malloc = do
  mov rax rbx
  add rbx rdi

-- Result in rax
readByte :: Code
readByte = do
  pushCallerSaved

  mov xmm0 xmm1

  popCallerSaved

-- Result in rax
peekByte :: Code
peekByte = do
  pushCallerSaved

  mov xmm0 xmm2

  popCallerSaved

-- Arg in rdi
writeByte :: Code
writeByte = do
  pushCallerSaved

  mov xmm0 xmm3

  popCallerSaved

compileStr :: String -> Code
compileStr str = mdo
  let len = fromIntegral $ length str

  -- Allocate the memory
  push rdi
  mov rdi $ ImmOp $ Immediate len
  malloc
  pop rdi

  -- Length at beginning of allocation
  mov (addr64 (fromReg rax)) (ImmOp (Immediate len))

  -- Put each character onto the allocation
  _ <- mapM (\c -> add rax 8 >> mov (addr64 (fromReg rax)) (ImmOp (Immediate (valueToBits (Char c))))) str
  return ()

compileExpr :: Expr -> Code
compileExpr expr = mdo
  case expr of
    _ -> compileDatum expr

compileOp0 :: Op0 -> Code
compileOp0 op = do
  case op of
    ReadByte -> readByte
    PeekByte -> peekByte

compileOp1 :: Op1 -> Code
compileOp1 op = do
  case op of
    -- TODO: type assertions
    Add1 -> inc rax
    Sub1 -> dec rax

compileDatum :: Expr -> Code
compileDatum expr = mdo
  case expr of
    Int val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Int val
    Bool val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Bool val
    Char val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Char val
    Str str -> compileStr str
    Eof -> mov rax $ ImmOp $ Immediate $ valueToBits Eof
