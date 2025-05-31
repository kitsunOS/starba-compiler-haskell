module IR.IRConstProp (propogateConstants) where

import qualified IR.IR as IR
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set

type LastValues = Map.Map IR.RegName (Map.Map IR.LabelRef IR.Value)

propogateConstants :: IR.Module -> IR.Module
propogateConstants (IR.Module procedures fieldTable symbolTable) =
  IR.Module (map propogateConstantsProcedure procedures) fieldTable symbolTable
  where
  propogateConstantsProcedure :: IR.Procedure -> IR.Procedure
  propogateConstantsProcedure (IR.Procedure blocks) = IR.Procedure $ propogateConstantsBlocks blocks
  propogateConstantsBlocks :: [IR.Block] -> [IR.Block]
  -- TODO: Iterate until fixpoint
  propogateConstantsBlocks blocks = fst $ propogateConstantsBlocks1 blocks Map.empty
  propogateConstantsBlocks1 :: [IR.Block] -> LastValues -> ([IR.Block], LastValues)
  propogateConstantsBlocks1 blocks lastValues = foldl (
      \(bs, lvs) b -> let (b', lvs') = propogateConstantsBlock b lvs in (bs ++ [b'], lvs')
    ) ([], lastValues) blocks
  propogateConstantsBlock :: IR.Block -> LastValues -> (IR.Block, LastValues)
  propogateConstantsBlock (IR.Block label instructions) lastValues =
    let (instructions', lastValues') = foldl (
            \(instrs, lvs) instr -> let (instrs', lvs') = propogateConstantsInstruction instr label lvs in (instrs ++ instrs', lvs')
          ) ([], lastValues) instructions
    in (IR.Block label instructions', lastValues')

propogateConstantsInstruction :: IR.Instruction -> IR.LabelRef -> LastValues -> ([IR.Instruction], LastValues)
propogateConstantsInstruction (IR.Set (IR.Register reg) val) label lastValues =
  case resolveConstant val label lastValues of
    Just v -> ([], appendLastValue reg v label lastValues)
    Nothing -> ([IR.Set (IR.Register reg) val], lastValues)
propogateConstantsInstruction (IR.Phi reg pairs) label lastValues =
  let newPairs = map (\(lr, v) -> (lr, optimizeValue v lr lastValues)) pairs
      constants = map snd newPairs
      uniqueConstants = Set.fromList constants
  in case Set.size uniqueConstants of
    0 -> ([], lastValues)
    1 -> ([], appendLastValue reg (Set.elemAt 0 uniqueConstants) label lastValues)
    _ -> ([IR.Phi reg newPairs], lastValues)
propogateConstantsInstruction (IR.BinOp op (IR.Register dest) src1 src2) label lastValues =
  let mValA = resolveConstant src1 label lastValues
      mValB = resolveConstant src2 label lastValues
      newValA = fromMaybe src1 mValA
      newValB = fromMaybe src2 mValB
      resultIsConstant = isJust mValA && isJust mValB
      newValue = case (mValA, mValB) of
        (Just a, Just b) -> evaluateBinOp op a b
        _ -> Nothing
  in case newValue of
    Just v -> ([], appendLastValue dest v label lastValues)
    Nothing -> ([IR.BinOp op (IR.Register dest) newValA newValB], lastValues)
propogateConstantsInstruction i _ lv = ([i], lv)

evaluateBinOp :: IR.BinOpType -> IR.Value -> IR.Value -> Maybe IR.Value
evaluateBinOp IR.Add (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (a + b)
evaluateBinOp IR.Sub (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (a - b)
evaluateBinOp IR.Mul (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (a * b)
evaluateBinOp IR.Div (IR.Immediate a) (IR.Immediate b) = if b /= 0 then Just $ IR.Immediate (a `div` b) else Nothing
-- TODO: Eventually, a proper boolean type
evaluateBinOp IR.Eq (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (if a == b then 1 else 0)
evaluateBinOp IR.Lt (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (if a < b then 1 else 0)
evaluateBinOp IR.Gt (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (if a > b then 1 else 0)
evaluateBinOp IR.Le (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (if a <= b then 1 else 0)
evaluateBinOp IR.Ge (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (if a >= b then 1 else 0)
evaluateBinOp IR.Ne (IR.Immediate a) (IR.Immediate b) = Just $ IR.Immediate (if a /= b then 1 else 0)
evaluateBinOp _ _ _ = Nothing

-- TODO: What about user-defined constants?
resolveConstant :: IR.Value -> IR.LabelRef -> LastValues -> Maybe IR.Value
resolveConstant (IR.Register v) lr lastValues =
  case Map.lookup v lastValues of
    Nothing -> Nothing
    Just m -> Map.lookup lr m
resolveConstant (IR.Immediate i) _ _ = Just $ IR.Immediate i
resolveConstant _ _ _ = Nothing

optimizeValue :: IR.Value -> IR.LabelRef -> LastValues -> IR.Value
optimizeValue val label lastValues = fromMaybe val (resolveConstant val label lastValues)

appendLastValue :: IR.RegName -> IR.Value -> IR.LabelRef -> LastValues -> LastValues
appendLastValue reg val label lastValues =
  let m = fromMaybe Map.empty (Map.lookup reg lastValues)
      m' = Map.insert label val m
  in Map.insert reg m' lastValues