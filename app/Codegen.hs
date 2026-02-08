{-# LANGUAGE RecursiveDo #-}

module Codegen where
import AST (Expr (..))
import CodeGen.X86
import CodeGen.X86.Asm
import Data.Int
import System.Process (callCommand)

type Ctx = Integer

build :: Code -> IO ()
build c = do
  writeFile "racket-test.o" (show c) 
  callCommand "ld racket-test.o"

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

-- Puts result in rax as usual
malloc :: Int64 -> Code
malloc len = do
  push rdi
  mov rdi $ ImmOp $ Immediate len
  pushCallerSaved

  -- TODO: actually call malloc :)
  nop

  popCallerSaved
  pop rdi

compileStr :: String -> Code
compileStr str = mdo
  let len = fromIntegral $ length str
  malloc len
  mov (addr64 (fromReg rax)) (ImmOp (Immediate len))

compileExpr :: Expr -> Code
compileExpr expr = mdo
  case expr of
    Int val -> mov rax $ ImmOp $ Immediate 5
    Bool val -> mov rax $ ImmOp $ Immediate 6
    Char char -> mov rax $ ImmOp $ Immediate 7
    Str str -> compileStr str
