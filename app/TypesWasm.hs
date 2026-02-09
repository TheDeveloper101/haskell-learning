{-# LANGUAGE BinaryLiterals #-}

module TypesWasm where

import Data.Bits
import Control.Conditional (cond)
import AST
import Data.Char (ord, chr)

immShift :: Int
immShift = 3
immMask :: Int
immMask = 0b111
ptrMask :: Int
ptrMask = 0b111
typeBox :: Int
typeBox = 0b001
typeCons :: Int
typeCons = 0b010
typeVect :: Int
typeVect = 0b011
typeStr :: Int
typeStr = 0b100
intShift :: Int
intShift = 1 + immShift
maskInt :: Int
maskInt = 0b1111
charShift :: Int
charShift = 2 + immShift
typeInt :: Int
typeInt = 0b0000
typeChar :: Int
typeChar = 0b01000
maskChar :: Int
maskChar = 0b11111

valueToBits :: Expr -> Int
valueToBits e = case e of
    Int i -> shiftL (fromIntegral i) intShift
    Bool b -> (if b then 0b00111000 else 0b00011000)
    Eof -> 0b01011000
    Char c -> fromIntegral typeChar .|. shiftL (fromIntegral (ord c)) charShift
    Empty -> 0b10011000
    _ -> -1

bitsToValue :: Int -> Expr

bitsToValue b = cond [(valueToBits (Bool True) == b, Bool True )
                  , (valueToBits (Bool False) == b, Bool False )
                  , (valueToBits Eof == b, Eof )
                  , (bitsIsInt b, Int (shiftR (fromIntegral b) intShift))
                  , (bitsIsChar b, Char (chr (shiftR (fromIntegral b) charShift)))
                  , (otherwise , Eof)]

bitsIsInt :: Int -> Bool
bitsIsInt b = typeInt == (b .&. maskInt)

bitsIsChar :: Int -> Bool
bitsIsChar b = typeChar == (b .&. maskChar)

bitsIsImm :: Int -> Bool
bitsIsImm b = 0 == (b .&. immMask)
