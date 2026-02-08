module Main where

import System.Environment
import System.IO

import Codegen
import CodegenWasm
import Parser

main :: IO ()
main = do
  putStr "> "
  hFlush stdout
  input <- getLine
  args <- getArgs
  case parseExpr input of
    Left err -> putStrLn err
    Right ast -> case head args of
      "x86" -> compileAndLink ast
      "wasm" -> runWasm ast >>= print
      _ -> putStrLn "bad backend :("
