{-# LANGUAGE RecursiveDo #-}

module Codegen where
import AST (Expr)
import CodeGen.X86 (jmp, nop, Code, CodeM, ret, label, j)
import CodeGen.X86.Asm (Label(Label))
import System.Process (callCommand)
import System.IO (hFlush)

type Ctx = Integer

build :: Code -> IO ()
build c = do
  writeFile "racket-test.o" (show c) 
  callCommand "ld racket-test.o"

compileTest :: Code
compileTest = do
  ret
  nop
  ret

compileExpr :: Expr -> Code
compileExpr expr = mdo
  nop
  ret
  l1 <- label
  compileTest
  jmp l4
  l2 <- label
  jmp l2
  l3 <- label
  jmp l3
  l4 <- label
  jmp l4
