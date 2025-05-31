{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn, hPrint)

import Text.Parsec

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)

import Parser
import AST.AST
import IR.IRGen (compileModule)
import qualified X86.X86Gen as X86Gen
import qualified X86.X86Nasm as X86Nasm
import Data.Bifunctor (first)
import qualified RegAlloc
import qualified IR.IR as IR
import qualified Data.Set as Set
import qualified X86.X86Asm as X86Asm
import qualified X86.X86Reg as X86Reg
import qualified IR.IRPhiElim
import qualified Data.Map as Map
import qualified AST.ASTSymbolRes as ASTSymbolRes
import qualified IR.IRPhiGen
import qualified IR.IRPhiGen as IRPhiGen
import qualified IR.IRConstProp as IRConstProp

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

  rawAst <- ExceptT $ pure $ first show $ parse parseModule filename contents
  let ast = ASTSymbolRes.resolveSymbols rawAst

  liftIO $ print ast
  liftIO $ print ""

  irLowered <- ExceptT $ pure $ compileModule ast
  liftIO $ print irLowered
  liftIO $ print "(irLowered)"

  irPhi <- ExceptT $ pure $ IRPhiGen.phiGen irLowered
  liftIO $ print irPhi
  liftIO $ print "(irPhi)"

  let irConstProp = IRConstProp.propogateConstants irPhi
  liftIO $ print irConstProp
  liftIO $ print "(irConstProp)"

  let irFinal = IR.IRPhiElim.rewriteModule irConstProp
  liftIO $ print irFinal
  liftIO $ print "(irFinal)"

  let ctx = RegAlloc.RegAllocContext X86Reg.intLive X86Reg.regCompat

  let allInOut :: Map.Map IR.LabelRef (RegAlloc.BlockInOut X86Asm.Register32)
      allInOut = RegAlloc.blocksLiveInOut2 (irBlocks irFinal)
  liftIO $ putStrLn (showAll allInOut)

  let liveness :: [RegAlloc.LiveSets X86Asm.Register32]
      liveness = map (RegAlloc.liveSets ctx allInOut) (irBlocks irFinal)
  liftIO $ print liveness
  liftIO $ print ""

  let interferences = foldl (\m (b, l) -> Map.unionWith Set.union m (RegAlloc.interferences ctx b l)) Map.empty (zip (irBlocks irFinal) liveness)
  liftIO $ print interferences
  liftIO $ print ""

  let compatMap = foldl (\m b -> Map.unionWith Set.union m (RegAlloc.compatMap ctx b)) Map.empty (irBlocks irFinal)
  liftIO $ print compatMap
  liftIO $ print ""

  let allocatedRegisters = RegAlloc.allocateRegisters ctx (irBlocks irFinal)
  liftIO $ print allocatedRegisters
  liftIO $ print ""

  let generationContext = X86Gen.GenerationContext allocatedRegisters
  x86 <- ExceptT $ pure $ X86Gen.generateAsm generationContext irFinal

  let nasmStr = X86Nasm.toNasmStr x86

  liftIO $ putStrLn nasmStr
  liftIO $ print ""
  liftIO $ writeFile outname nasmStr

irBlocks :: IR.Module -> [IR.Block]
irBlocks (IR.Module (IR.Procedure blocks : _) _ _) = blocks
irBlocks _ = error "No blocks in module"

showAll :: (Show a) => Map.Map IR.LabelRef a -> String
showAll m = unlines $ map (\(k, v) -> show k ++ ": " ++ show v) (Map.toList m)