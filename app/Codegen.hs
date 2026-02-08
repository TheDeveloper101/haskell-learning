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
    Prim1 op expr -> compileOp1 op expr
    _ -> compileDatum expr

compileOp0 :: Op0 -> Code
compileOp0 op = do
  case op of
    ReadByte -> readByte
    PeekByte -> peekByte

compileOp1 :: Op1 -> Expr -> Code
compileOp1 op = undefined --do
  --case op of
    -- TODO: type assertions
    --Add1 -> inc rax
    --Sub1 -> dec rax

compileDatum :: Expr -> Code
compileDatum expr = mdo
  case expr of
    Int val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Int val
    Bool val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Bool val
    Char val -> mov rax $ ImmOp $ Immediate $ valueToBits $ Char val
    Str str -> compileStr str
    Eof -> mov rax $ ImmOp $ Immediate $ valueToBits Eof

-- compileOp0 :: Expr -> Code
-- compileOp0 = undefined

-- compileOp1 :: Op1 -> Expr -> Code
--compileOp1 op expr = mdo
--   compileExpr expr
--   case op of
--     Add1 -> add rax $ ImmOp $ Immediate $ fromIntegral $ valueToBits $ Int 1
--     _ -> error "todo"
-- compileOp1 Add1 val = undefined
-- compileOp1 Sub1 val = undefined
-- compileOp1 ZeroHuh val = undefined
-- compileOp1 CharHuh val = undefined
-- compileOp1 IntegerToChar val = undefined
-- compileOp1 CharToInteger val = undefined
-- compileOp1 WriteByte val = undefined
-- compileOp1 EofObjectHuh val = undefined
-- compileOp1 Box val = undefined
-- compileOp1 Car val = undefined
-- compileOp1 Cdr val = undefined
-- compileOp1 Unbox val = undefined
-- compileOp1 EmptyHuh val = undefined
-- compileOp1 ConsHuh val = undefined
-- compileOp1 BoxHuh val = undefined
-- compileOp1 VectorHuh val = undefined
-- compileOp1 VectorLength val = undefined
-- compileOp1 StringHuh val = undefined
-- compileOp1 StringLength val = undefined

compileOp2 :: Expr -> Code
compileOp2 = undefined