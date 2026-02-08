{-# LANGUAGE BinaryLiterals #-}

module Types where

import Data.Bits
import Control.Conditional (cond)
import AST
import Data.Char (ord, chr)
import Data.Int (Int64)

immShift :: Int
immShift = 3
immMask :: Int64
immMask = 0b111
ptrMask :: Int64
ptrMask = 0b111
typeBox :: Integer
typeBox = 0b001
typeCons :: Integer
typeCons = 0b010
typeVect :: Integer
typeVect = 0b011
typeStr :: Integer
typeStr = 0b100
intShift :: Int
intShift = 1 + immShift
maskInt :: Int64
maskInt = 0b1111
charShift :: Int
charShift = 2 + immShift
typeInt :: Int64
typeInt = 0b0000
typeChar :: Int64
typeChar = 0b01000
maskChar :: Int64
maskChar = 0b11111

valueToBits :: Expr -> Int64
valueToBits e = case e of
    Int i -> shiftL (fromIntegral i) intShift
    Bool b -> (if b then 0b00011000 else 0b00111000)
    Eof -> 0b01011000
    Char c -> fromIntegral typeChar .|. shiftL (fromIntegral (ord c)) charShift
    Empty -> 0b10011000
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
