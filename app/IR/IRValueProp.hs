module IR.IRValueProp (propogateValues) where

import qualified IR.IR as IR
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set

type LastValues = Map.Map IR.RegName (Map.Map IR.LabelRef IR.Value)

propogateValues :: IR.Module -> IR.Module
propogateValues (IR.Module procedures fieldTable symbolTable) =
  IR.Module (map propogateValuesProcedure procedures) fieldTable symbolTable
  where
  propogateValuesProcedure :: IR.Procedure -> IR.Procedure
  propogateValuesProcedure (IR.Procedure blocks) = IR.Procedure $ propogateValuesBlocks blocks
  propogateValuesBlocks :: [IR.Block] -> [IR.Block]
  -- TODO: Iterate until fixpoint
  propogateValuesBlocks blocks = go blocks Map.empty
    where
      go newBlocks lastValues =
        let (newBlocks', lastValues') = propogateValuesBlocks1 newBlocks lastValues
        in if newBlocks' == newBlocks
             then newBlocks'
             else go newBlocks' lastValues'
  propogateValuesBlocks1 :: [IR.Block] -> LastValues -> ([IR.Block], LastValues)
  propogateValuesBlocks1 blocks lastValues = foldl (
      \(bs, lvs) b -> let (b', lvs') = propogateValuesBlock b lvs in (bs ++ [b'], lvs')
    ) ([], lastValues) blocks
  propogateValuesBlock :: IR.Block -> LastValues -> (IR.Block, LastValues)
  propogateValuesBlock (IR.Block label instructions) lastValues =
    let (instructions', lastValues') = foldl (
            \(instrs, lvs) instr -> let (instrs', lvs') = propogateValuesInstruction instr label lvs in (instrs ++ instrs', lvs')
          ) ([], lastValues) instructions
    in (IR.Block label instructions', lastValues')

propogateValuesInstruction :: IR.Instruction -> IR.LabelRef -> LastValues -> ([IR.Instruction], LastValues)
propogateValuesInstruction (IR.Set (IR.Register reg) val) label lastValues =
  -- Set will hopefully always have the effect of making the new register equal an old, different value
  ([], appendLastValue reg (optimizeValue val label lastValues) label lastValues)
propogateValuesInstruction (IR.Phi reg pairs) label lastValues =
  let newPairs = map (\(lr, v) -> (lr, optimizeValue v lr lastValues)) pairs
      values = map snd newPairs
      uniqueValues = Set.fromList values
  in case Set.size uniqueValues of
    0 -> ([], lastValues)
    1 -> ([], appendLastValue reg (Set.elemAt 0 uniqueValues) label lastValues)
    _ -> ([IR.Phi reg newPairs], lastValues)
propogateValuesInstruction (IR.BinOp op (IR.Register dest) src1 src2) label lastValues =
  let newValA = optimizeValue src1 label lastValues
      newValB = optimizeValue src2 label lastValues
      newValue = evaluateBinOp op newValA newValB
  in case newValue of
    Just v -> ([], appendLastValue dest v label lastValues)
    Nothing -> ([IR.BinOp op (IR.Register dest) newValA newValB], lastValues)
propogateValuesInstruction i _ lv = ([i], lv)

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
resolveValue :: IR.Value -> IR.LabelRef -> LastValues -> Maybe IR.Value
resolveValue (IR.Register v) lr lastValues =
  case Map.lookup v lastValues of
    Nothing -> Nothing
    Just m -> Map.lookup lr m
resolveValue (IR.Immediate i) _ _ = Just $ IR.Immediate i
resolveValue _ _ _ = Nothing

optimizeValue :: IR.Value -> IR.LabelRef -> LastValues -> IR.Value
optimizeValue val label lastValues = fromMaybe val (resolveValue val label lastValues)

appendLastValue :: IR.RegName -> IR.Value -> IR.LabelRef -> LastValues -> LastValues
appendLastValue reg val label lastValues = Map.unionWith Map.union lastValues (Map.singleton reg (Map.singleton label val))