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
    captureBlock (IR.Block label instructions) = Map.singleton label (Map.unionsWith Set.union (map capturePhis instructions))
    capturePhis :: IR.Instruction -> Map.Map IR.LabelRef (Set.Set (IR.RegName, IR.RegName))
    capturePhis (IR.Phi destReg args) =
      foldl (\m (innerLabel, srcReg) ->
        Map.insertWith Set.union innerLabel (Set.singleton (destReg, srcReg)) m
      ) Map.empty args
    capturePhis _ = Map.empty

rewriteProcedure :: IR.Procedure -> PhiCaptures -> IR.Procedure
rewriteProcedure (IR.Procedure blocks) captures = IR.Procedure (map (rewriteBlock captures) blocks)
  where
    rewriteBlock :: PhiCaptures -> IR.Block -> IR.Block
    rewriteBlock captures (IR.Block label instructions) = IR.Block label (rewriteInstructions captures label instructions)
    rewriteInstructions :: PhiCaptures -> IR.LabelRef -> [IR.Instruction] -> [IR.Instruction]
    rewriteInstructions captures label = concatMap (rewriteInstruction captures label)
    rewriteInstruction :: PhiCaptures -> IR.LabelRef -> IR.Instruction -> [IR.Instruction]
    rewriteInstruction captures innerLabel instruction = case instruction of
      IR.Phi destReg args -> []
      IR.Jmp outerLabel ->
          map (\(destReg, srcReg) -> IR.Set (IR.Register destReg) (IR.Register srcReg))
            (Set.toList (Map.findWithDefault Set.empty innerLabel (Map.findWithDefault Map.empty outerLabel captures)))
          ++ [IR.Jmp outerLabel]
      IR.JmpIf cond trueLabel falseLabel ->
          -- TODO: Could this be smarter and only set when the jump will actually occur?
          map (\(destReg, srcReg) -> IR.Set (IR.Register destReg) (IR.Register srcReg))
            (Set.toList (Map.findWithDefault Set.empty innerLabel (Map.findWithDefault Map.empty trueLabel captures)))
          ++ map (\(destReg, srcReg) -> IR.Set (IR.Register destReg) (IR.Register srcReg))
            (Set.toList (Map.findWithDefault Set.empty innerLabel (Map.findWithDefault Map.empty falseLabel captures)))
          ++ [IR.JmpIf cond trueLabel falseLabel]
      _ -> [instruction]