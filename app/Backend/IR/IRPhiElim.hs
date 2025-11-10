module Backend.IR.IRPhiElim (rewriteModule) where


import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified GHC.Event as Map

import qualified Backend.IR.IR as IR

type PhiCaptures = Map.Map IR.LabelRef (Map.Map IR.LabelRef (Set.Set (IR.RegName, IR.Value)))
type RegSets = Map.Map IR.RegName (Set.Set IR.Value)

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
      foldl (\m (innerLabel, srcVal) ->
        Map.unionWith (Map.unionWith Set.union) m (Map.singleton innerLabel (Map.singleton outerLabel (Set.singleton (destReg, srcVal))))
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
    genRegSets :: PhiCaptures -> IR.LabelRef -> IR.LabelRef -> RegSets
    genRegSets captures outerLabel innerLabel =
      case Map.lookup outerLabel captures of
        Nothing -> Map.empty
        Just innerMap -> foldr (\(_, s) acc -> foldl (
          \acc2 (destReg, srcVal) -> if IR.Register destReg == srcVal
            then acc2
            else Map.insertWith Set.union destReg (Set.singleton srcVal) acc2) acc s
          ) Map.empty $ Map.toList innerMap
    genSets :: PhiCaptures -> IR.LabelRef -> IR.LabelRef -> [IR.Instruction]
    genSets captures outerLabel innerLabel = genSets' (genRegSets captures outerLabel innerLabel) Set.empty
    -- TODO: Don't ignore the innerLabel
    genSets' :: RegSets -> Set.Set IR.RegName -> [IR.Instruction]
    genSets' regSets completed =
      let
        currentRegSets = filterNextRegs regSets completed
        completed' = Set.union completed (Map.keysSet currentRegSets)
        newInstructions = Map.foldrWithKey (\reg deps acc ->
          if Set.null deps
            then acc
            else IR.Set (IR.Register reg) (Set.elemAt 0 deps) : acc) [] currentRegSets
      in
        if Set.null (Map.keysSet currentRegSets)
          then newInstructions
          else genSets' regSets completed' ++ newInstructions

filterNextRegs :: RegSets -> Set.Set IR.RegName -> RegSets
filterNextRegs regSets completed =
  let
    incompleteRegSets = Map.filterWithKey (\reg _ -> not (Set.member reg completed)) regSets
  in Map.filter (\deps -> Set.null $ regSet deps `Set.intersection` Map.keysSet regSets `Set.difference` completed) incompleteRegSets
  where
    regSet :: Set.Set IR.Value -> Set.Set IR.RegName
    regSet = Set.foldr (\v acc -> case v of
      IR.Register reg -> Set.insert reg acc
      _ -> acc) Set.empty