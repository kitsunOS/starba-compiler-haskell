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
import qualified X86.X86Gen as X86Gen
import qualified X86.X86Nasm as X86Nasm
import Data.Bifunctor (first)
import qualified RegAlloc
import qualified IR
import qualified Data.Set as Set
import qualified X86.X86Asm as X86Asm
import qualified X86.X86Reg as X86Reg

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

  let ctx = RegAlloc.RegAllocContext X86Reg.intLive
  let liveness :: [RegAlloc.LiveSets X86Asm.Register32]
      liveness = map (RegAlloc.liveSets ctx . IR.blockInstructions) (irBlocks ir)
  liftIO $ print liveness
  liftIO $ print ""

  let interferences = RegAlloc.interferences (head $ irBlocks ir) (head liveness)
  liftIO $ print interferences
  liftIO $ print ""

  let allocatedRegisters = RegAlloc.allocateRegisters ctx (irBlocks ir) (Set.fromList [X86Asm.EAX, X86Asm.EBX, X86Asm.ECX, X86Asm.EDX])
  liftIO $ print allocatedRegisters
  liftIO $ print ""

  let generationContext = X86Gen.GenerationContext allocatedRegisters
  x86 <- ExceptT $ pure $ X86Gen.generateAsm generationContext ir

  let nasmStr = X86Nasm.toNasmStr x86

  liftIO $ putStrLn nasmStr
  liftIO $ print ""
  liftIO $ writeFile outname nasmStr

irBlocks :: IR.Module -> [IR.Block]
irBlocks (IR.Module (IR.Procedure blocks : _) _ _) = blocks
irBlocks _ = error "No blocks in module"
