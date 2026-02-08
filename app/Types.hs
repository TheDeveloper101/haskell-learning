{-# LANGUAGE BinaryLiterals #-}

module Types where

import Data.Bits
import Control.Conditional (cond)
import AST
import Data.Char (ord, chr)
import Data.Int (Int64)

immShift, immMask, ptrMask, typeBox, typeCons, typeVect, typeStr, intShift, maskInt, charShift, typeInt, typeChar, maskChar :: Int64
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

valueToBits :: Expr -> Int64
valueToBits e = case e of
    Int i -> shiftL (fromIntegral i) intShift
    Bool b -> (if b then 0b00011000 else 0b00111000)
    Eof -> 0b01011000
    Char c -> fromIntegral typeChar .|. shiftL (fromIntegral (ord c)) charShift
    _ -> -1

bitsToValue :: Int64 -> Expr

bitsToValue b = cond [(valueToBits (Bool True) == b, Bool True )
                  , (valueToBits (Bool False) == b, Bool False )
                  , (valueToBits Eof == b, Eof )
                  , (bitsIsInt b, Int (shiftR (fromIntegral b) intShift))
                  , (bitsIsChar b, Char (chr (shiftR (fromIntegral b) charShift)))
                  , (otherwise , Eof)]

bitsIsInt :: Int64 -> Bool
bitsIsInt b = typeInt == (b .&. maskInt)

bitsIsChar :: Int64 -> Bool
bitsIsChar b = typeChar == (b .&. maskChar)

bitsIsImm :: Int64 -> Bool
bitsIsImm b = 0 == (b .&. immMask)
