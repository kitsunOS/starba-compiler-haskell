{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn, hPrint)

import Text.Parsec

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)

import Parser
import AST
import IRGen (compileModule)
import X86.X86Gen (generateAsm)
import X86.X86Nasm (toNasmStr)
import Data.Bifunctor (first)
import qualified X86.X86RegAlloc as X86RegAlloc
import qualified IR

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename, outname] -> do
      result <- runExceptT $ run filename outname

      case result of
        Left err -> hPutStrLn stderr err
        Right _ -> pure ()
    _ -> hPutStrLn stderr "Usage: starba <filename> <outname>"

run :: String -> String -> ExceptT String IO ()
run filename outname = do
  contents <- liftIO $ readFile filename

  ast <- ExceptT $ pure $ first show $ parse parseModule filename contents

  liftIO $ print ast
  liftIO $ print ""

  ir <- ExceptT $ pure $ compileModule ast

  liftIO $ print ir
  liftIO $ print ""

  let liveness = map (X86RegAlloc.liveSets . IR.blockInstructions) (irBlocks ir)
  liftIO $ print liveness
  liftIO $ print ""

  let interferences = X86RegAlloc.interferences (head $ irBlocks ir) (head liveness)
  liftIO $ print interferences
  liftIO $ print ""

  let allocatedRegisters = X86RegAlloc.allocateRegisters (irBlocks ir)
  liftIO $ print allocatedRegisters
  liftIO $ print ""

  x86 <- ExceptT $ pure $ generateAsm ir

  let nasmStr = toNasmStr x86

  liftIO $ putStrLn nasmStr
  liftIO $ print ""
  liftIO $ writeFile outname nasmStr

irBlocks :: IR.Module -> [IR.Block]
irBlocks (IR.Module (IR.Procedure blocks : _) _ _) = blocks
irBlocks _ = error "No blocks in module"
