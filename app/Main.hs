{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map

import System.Environment (getArgs)
import System.IO (stderr, hPutStrLn, hPrint)

import Text.Parsec

import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)

import Data.Bifunctor (first)

import qualified Frontend.Starba.Parse.Parser as Parser
import qualified Frontend.Starba.AST.AST as AST
import qualified Frontend.Starba.Codegen.CodegenMain as Codegen
import qualified Frontend.Starba.AST.ASTSymbolRes as ASTSymbolRes

import qualified Backend.Reg.RegAlloc as RegAlloc
import qualified Backend.IR.IR as IR
import qualified Backend.IR.IRPhiElim as IRPhiElim
import qualified Backend.IR.IRPhiGen as IRPhiGen
import qualified Backend.Opt.IRValueProp as IRValueProp
import qualified Backend.Analysis.IRCfgAnalysis as IRCA

import qualified Target.X86.X86Gen as X86Gen
import qualified Target.X86.X86Nasm as X86Nasm
import qualified Target.X86.X86Reg as X86Reg

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

  rawAst <- ExceptT $ pure $ first show $ parse Parser.parseModule filename contents
  let ast = ASTSymbolRes.resolveSymbols rawAst

  liftIO $ print ast
  liftIO $ print ""

  irLowered <- ExceptT $ pure $ Codegen.compileModule ast
  liftIO $ print irLowered
  liftIO $ print "(irLowered)"

  liftIO $ print (map IRCA.generateCfg $ IR.moduleProcedures irLowered)
  liftIO $ print "(cfgAnalysis)"

  irPhi <- mapProcedures IRPhiGen.phiGen irLowered
  liftIO $ print irPhi
  liftIO $ print "(irPhi)"

  irValueProp <- mapProcedures IRValueProp.propogateValues irPhi
  liftIO $ print irValueProp
  liftIO $ print "(irValueProp)"

  let irFinal = IRPhiElim.rewriteModule irValueProp
  liftIO $ print irFinal
  liftIO $ print "(irFinal)"

  let ctx = RegAlloc.RegAllocContext X86Reg.intLive X86Reg.regCompat

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

mapProcedures :: (IR.Procedure -> Either String IR.Procedure) -> IR.Module -> ExceptT String IO IR.Module
mapProcedures transform mod = do
  let moduleProcs = IR.moduleProcedures mod
  let transformedProcsE = traverse transform moduleProcs
  newProcs <- ExceptT $ pure transformedProcsE
  pure $ mod { IR.moduleProcedures = newProcs }