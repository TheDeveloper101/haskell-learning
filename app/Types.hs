{-# LANGUAGE BinaryLiterals #-}

module Types (valueToBits, bitsToValue) where

import Data.Bits
import Control.Conditional (cond)
import AST
import Data.Char (ord, chr)

immShift = 3
immMask = 0b111
ptrMask = 0b111
typeBox = 0b001
typeCons = 0b010
typeVect = 0b011
typeStr = 0b100
intShift = 1 + immShift
maskInt = 0b1111
charShift = 2 + immShift
typeInt = 0b0000
typeChar = 0b01000
maskChar = 0b11111

valueToBits :: Expr -> Int

valueToBits e = case e of
    Int i -> shiftL i intShift
    Bool b -> (if b then 0b00011000 else 0b00111000)
    Eof -> 0b01011000
    Char c -> typeChar .|. shiftL charShift (ord c)
    _ -> -1

bitsToValue :: Int -> Expr

bitsToValue b = cond [(valueToBits (Bool True) == b, Bool True )
                  , (valueToBits (Bool False) == b, Bool False )
                  , (valueToBits Eof == b, Eof )
                  , (bitsIsInt b, Int (shiftR b intShift))
                  , (bitsIsChar b, Char (chr (shiftR b charShift)))
                  , (otherwise , Eof)]

bitsIsInt :: Int -> Bool
bitsIsInt b = typeInt == (b .&. maskInt)

bitsIsChar :: Int -> Bool
bitsIsChar b = typeChar == (b .&. maskChar)

bitsIsImm :: Int -> Bool
bitsIsImm b = 0 == (b .&. immMask)