{-# LANGUAGE RecursiveDo #-}

module Codegen where

import AST
import CodeGen.X86
import CodeGen.X86.Asm
import Data.Text (pack, replace, unpack)
import Data.Int
import System.Process (callCommand)
import Types
import Assertx86
import Text.Megaparsec (MonadParsec(eof))

type Ctx = Integer

wrapBool :: Code
wrapBool = mdo
  cmp rax 0
  j E elsel
  mov rax $ ImmOp $ Immediate $ valueToBits $ Bool True
  elsel <- label
  mov rax $ ImmOp $ Immediate $ valueToBits $ Bool False

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

pushCalleeSaved :: Code
pushCalleeSaved = do
  push rbx
  push r12
  push r13
  push r14
  push r15

popCalleeSaved :: Code
popCalleeSaved = do
  pop r15
  pop r14
  pop r13
  pop r12
  pop rbx

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
    ZeroHuh -> do
      assertInt errLabel rax
      cmp rax 0
      ifEqual
    CharHuh -> do
      and_ rax $ ImmOp $ Immediate maskChar
      cmp rax $ ImmOp $ Immediate typeChar
      ifEqual
    CharToInteger -> do
      assertChar errLabel rax
      sar rax $ fromIntegral charShift
      shl rax $ fromIntegral intShift -- SAL and SHL share teh same opcode???
    IntegerToChar -> do
      --assertCodepoint
      sar rax $ fromIntegral intShift
      sar rax $ fromIntegral charShift
      xor_ rax $ fromIntegral typeChar
    EofObjectHuh -> do
      cmp rax $ ImmOp $ Immediate $ valueToBits Eof
      ifEqual
    WriteByte -> writeByte
    Box -> do
      mov (addr64 rbx) rax
      mov rax rbx
      xor_ rax $ fromIntegral typeBox
      add rbx 8
    Unbox -> do
      assertBox errLabel rax
      mov rax $ MemOp (Addr (Just rax) (Just $ fromIntegral $ - typeBox) NoIndex)
    Car -> do
      assertCons errLabel rax
      mov rax $ MemOp (Addr (Just rax) (Just $ fromIntegral $ - typeCons) NoIndex)
    Cdr -> do
      assertCons errLabel rax
      mov rax $ MemOp (Addr (Just rax) (Just $ fromIntegral $ 8 - typeCons) NoIndex)
    EmptyHuh -> do
      cmp rax $ ImmOp $ Immediate $ valueToBits Empty
    ConsHuh -> do
      typePred ptrMask $ fromIntegral typeCons
    BoxHuh -> do
      typePred ptrMask $ fromIntegral typeCons
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

typePred :: Int64 -> Int64 -> Code
typePred mask ty = do
  and rax mask
  cmp rax ty
  ifEqual

findAndReplace :: String -> String -> String -> String
findAndReplace orig new str = unpack $ replace (pack orig) (pack new) (pack str)

compileMain :: Expr -> Code
compileMain expr = mdo
  pushCalleeSaved
  compileExpr errLabel expr
  popCalleeSaved
  ret
  errLabel <- label
  mov xmm0 xmm4

compileProgramToAsm :: Expr -> String
compileProgramToAsm mainExpr =
  let mainCode = compileMain mainExpr
      code = show mainCode
      -- Remove hexdump + opcodes
      dropColsIf line = case line of
        '.':_ -> line ++ ":"
        _ -> (unwords . drop 2 . words) line
      fixedCode = (unlines . map (("  " ++) . dropColsIf) . lines) code
      -- Add prelude & epilogue
      prelude = "global main\n\
      \extern malloc\n\n\
      \extern read_byte\n\n\
      \extern peek_byte\n\n\
      \extern write_byte\n\n\
      \extern print_result\n\n\
      \section .text\n\n\
      \main:\n\
      \  push rdi\n\
      \  mov rdi, 100000\n\
      \  call malloc\n\
      \  pop rdi\n\
      \  mov rbx, rax\n\n"
      fixedCode' = prelude ++ fixedCode
      -- HACK: Replace xmm movs with appropriate calls :))
      replacements =
        [ ("mov xmm0, xmm1", "call read_byte"),
          ("mov xmm0, xmm2", "call peek_byte"),
          ("mov xmm0, xmm3", "call write_byte"),
          ("mov xmm0, xmm4", "mov rdi, 255\n  mov rax, 60\n  syscall")
        ]
      fixedCode'' = foldl (\acc (orig, new) -> findAndReplace orig new acc) fixedCode' replacements
   in fixedCode''

compileAndLink :: Expr -> IO ()
compileAndLink mainExpr = do
  let file = compileProgramToAsm mainExpr
  writeFile "program.s" file
  callCommand "nasm -felf64 program.s && gcc runtime/*.c program.o -o program"
