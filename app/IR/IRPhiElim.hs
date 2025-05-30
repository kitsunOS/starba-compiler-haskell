module IR.IRPhiElim where

import qualified IR.IR as IR
import qualified Data.Map as Map
import qualified Data.Set as Set
type PhiCaptures = Map.Map IR.LabelRef (Map.Map IR.LabelRef (Set.Set (IR.RegName, IR.RegName)))

rewriteModule :: IR.Module -> IR.Module
rewriteModule (IR.Module procedures fieldTable symbolTable) =
  IR.Module (map (\p -> rewriteProcedure p (captureProcedure p)) procedures) fieldTable symbolTable

captureProcedure :: IR.Procedure -> PhiCaptures
captureProcedure (IR.Procedure blocks) = captureBlocks blocks
  where
    captureBlocks :: [IR.Block] -> PhiCaptures
    captureBlocks blocks = Map.unionsWith (Map.unionWith Set.union) (map captureBlock blocks)
    captureBlock :: IR.Block -> PhiCaptures
    captureBlock (IR.Block label instructions) = foldl (capturePhis label) Map.empty instructions
    capturePhis :: IR.LabelRef -> PhiCaptures -> IR.Instruction -> PhiCaptures
    capturePhis outerLabel captures (IR.Phi destReg args) =
      foldl (\m (innerLabel, srcReg) ->
        Map.unionWith (Map.unionWith Set.union) m (Map.singleton innerLabel (Map.singleton outerLabel (Set.singleton (destReg, srcReg))))
      ) captures args
    capturePhis _ captures _ = captures

rewriteProcedure :: IR.Procedure -> PhiCaptures -> IR.Procedure
rewriteProcedure (IR.Procedure blocks) captures = IR.Procedure (map (rewriteBlock captures) blocks)
  where
    rewriteBlock :: PhiCaptures -> IR.Block -> IR.Block
    rewriteBlock captures (IR.Block label instructions) = IR.Block label (rewriteInstructions captures label instructions)
    rewriteInstructions :: PhiCaptures -> IR.LabelRef -> [IR.Instruction] -> [IR.Instruction]
    rewriteInstructions captures label = concatMap (rewriteInstruction captures label)
    rewriteInstruction :: PhiCaptures -> IR.LabelRef -> IR.Instruction -> [IR.Instruction]
    rewriteInstruction captures outerLabel instruction = let gs = genSets captures outerLabel in case instruction of
      IR.Phi destReg args -> []
      IR.Jmp innerLabel -> gs innerLabel ++ [IR.Jmp innerLabel]
      IR.JmpIf cond trueLabel falseLabel -> gs trueLabel ++ gs falseLabel ++ [IR.JmpIf cond trueLabel falseLabel]
      _ -> [instruction]
    genSets :: PhiCaptures -> IR.LabelRef -> IR.LabelRef -> [IR.Instruction]
    genSets captures outerLabel innerLabel =
      -- TODO: Don't ignore the inner label
      case Map.lookup outerLabel captures of
        Nothing -> []
        Just innerMap -> foldl (\acc (_, s) -> foldl(
          \acc2 (destReg, srcReg) -> if destReg == srcReg
            then acc2
            else IR.Set (IR.Register destReg) (IR.Register srcReg):acc2) acc s
          ) [] $ Map.toList innerMap