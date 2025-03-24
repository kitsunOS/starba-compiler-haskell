module Main where

import System.Environment ( getArgs )
import System.IO ( stderr, hPutStrLn, hPrint )

import Text.Parsec

import Parser
import AST
import IRGen (compileModule)
import X86.X86Gen (generateAsm)
import X86.X86Nasm (toNasmStr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename, outname] -> do
      contents <- readFile filename
      case parse parseModule filename contents of
        Left err -> hPrint stderr err
        Right ast -> do
          print ast
          print ""
          case compileModule ast of
            Left err -> hPrint stderr err
            Right ir -> do
              print ir
              print ""
              case generateAsm ir of
                Left err -> hPrint stderr err
                Right x86 -> do
                  let nasmStr = toNasmStr x86
                  putStrLn nasmStr
                  print ""
                  writeFile outname nasmStr
    _ -> hPutStrLn stderr "Usage: starba <filename> <outname>"