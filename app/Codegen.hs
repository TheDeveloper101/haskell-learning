{-# LANGUAGE RecursiveDo #-}

module Codegen where

import AST
import CodeGen.X86
import CodeGen.X86.Asm
import Data.Text (pack, replace, unpack)
import System.Process (callCommand)
import Types
import Assertx86

type Ctx = Integer

build :: Code -> IO ()
build c = do
  writeFile "racket-test.o" (show c)
  callCommand "ld racket-test.o"

wrapBool :: Code
wrapBool = mdo
  cmp rax 0
  j E elsel
  mov rax $ ImmOp $ Immediate $ valueToBits $ Bool True
  elsel <- label
  mov rax $ ImmOp $ Immediate $ valueToBits $ Bool False

charHuh :: Code
charHuh = mdo
  and_ rax $ ImmOp $ Immediate maskChar
  cmp rax $ ImmOp $ Immediate typeChar
  ifEqual

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

compileExpr :: Label -> Expr -> Code
compileExpr errLabel expr = mdo
  case expr of
    Prim1 op expr1 -> compileOp1 errLabel op expr1
    Prim2 op expr1 expr2 -> compileOp2 errLabel op expr1 expr2
    _ -> compileDatum expr

compileDatum :: Expr -> Code
compileDatum expr = mdo
  case expr of
    Int val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Int val
    Bool val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Bool val
    Char val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Char val
    Str str -> compileStr str
    Eof -> mov rax $ ImmOp $ Immediate $ valueToBits Eof
    Prim0 op -> compileOp0 op

compileOp0 :: Op0 -> Code
compileOp0 ReadByte = readByte
compileOp0 PeekByte = peekByte

compileOp1 :: Label -> Op1 -> Expr -> Code
compileOp1 errLabel op expr = mdo
   compileExpr errLabel expr
   case op of
     Add1 -> add rax $ ImmOp $ Immediate $ valueToBits $ Int 1
     Sub1 -> sub rax $ ImmOp $ Immediate $ valueToBits $ Int 1
     CharHuh -> charHuh
      
     _ -> error "todo"

compileOp2 :: Label -> Op2 -> Expr -> Expr -> Code
compileOp2 errLabel op expr1 expr2 = mdo
  compileExpr errLabel expr1
  push rax
  compileExpr errLabel expr2
  case op of
    Plus -> do
      pop r8
      assertInt errLabel r8
      assertInt errLabel rax
      add rax r8
    Minus -> do
      pop r8
      assertInt errLabel r8
      assertInt errLabel rax
      sub r8 rax
      mov rax r8
    LessThan -> do
      pop r8
      assertInt errLabel r8
      assertInt errLabel rax
      cmp r8 rax
      ifLessThan
    Equals -> do
      pop r8
      assertInt errLabel r8
      assertInt errLabel rax
      cmp r8 rax
      ifEqual
    Cons -> do
      mov (MemOp $ Addr (Just rbx) (Just 8) NoIndex) rax
      pop rax
      mov (MemOp $ Addr (Just rbx) (Just 0) NoIndex) rax
      mov rax rbx
      xor_ rax $ ImmOp $ Immediate $ fromInteger typeCons
      add rbx 16
    EqHuh -> do
      pop r8
      cmp rax r8
      ifEqual
    MakeVector -> mdo
      pop r8
      assertNatural errLabel r8

      -- Special case for length = 0
      cmp (RegOp r8) $ ImmOp $ Immediate 0
      j NE nonzero

      -- Return canonical repr
      mov rax $ ImmOp $ Immediate $ fromIntegral typeVect
      jmp end

      -- Non-zero case
      nonzero <- label
      mov (addr64 $ fromReg rbx) r8
      sar r8 1
      mov (addr64 $ fromReg r9) r8

      -- Start intialization
      loop <- label
      mov (addr64 $ fromReg rbx) rax
      sub r8 8
      cmp r8 0
      j NE loop
      -- End initialization

      mov rax rbx
      xor_ rax $ ImmOp $ Immediate $ fromIntegral typeVect
      add rbx r9
      add rbx 8
      end <- label
      nop
    _ -> error "todo"

ifLessThan :: Code
ifLessThan = do
  mov rax $ ImmOp $ Immediate $ valueToBits $ Bool False
  mov r9 $ ImmOp $ Immediate $ valueToBits $ Bool True
  cmov L rax r9

ifEqual :: Code
ifEqual = do
  mov rax $ ImmOp $ Immediate $ valueToBits $ Bool False
  mov r9 $ ImmOp $ Immediate $ valueToBits $ Bool True
  cmov E rax r9

findAndReplace :: String -> String -> String -> String
findAndReplace orig new str = unpack $ replace (pack orig) (pack new) (pack str)

compileMain :: Expr -> Code
compileMain expr = mdo
  compileExpr errLabel expr
  ret
  errLabel <- label
  -- TODO: handle errors here
  ret

compileProgramToAsm :: Expr -> String
compileProgramToAsm mainExpr =
  let mainCode = compileMain mainExpr
      code = show mainCode
      -- HACK: Replace xmm movs with appropriate calls :))
      replacements =
        [ ("mov xmm0, xmm1", "call readByte"),
          ("mov xmm0, xmm2", "call peekByte"),
          ("mov xmm0, xmm3", "call writeByte")
        ]
      fixedCode = foldl (\acc (orig, new) -> findAndReplace orig new acc) code replacements
      -- Remove hexdump + opcodes
      dropColsIf line = case line of
        '.':_ -> line ++ ":"
        _ -> (unwords . drop 2 . words) line
      fixedCode' = (unlines . map (("  " ++) . dropColsIf) . lines) fixedCode
      -- Add prelude & epilogue
      prelude = "global main\n\
      \extern malloc\n\n\
      \section .text\n\n\
      \main:\n\
      \  push rdi\n\
      \  mov rdi, 100000\n\
      \  call malloc\n\
      \  pop rdi\n\
      \  mov rbx, rax\n\n"
      fixedCode'' = prelude ++ fixedCode'
   in fixedCode''
