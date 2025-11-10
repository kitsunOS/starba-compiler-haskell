module IR.IRValueProp (propogateValues) where

import qualified IR.IR as IR
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import qualified Debug.Trace as Trace

type LastValues = Map.Map IR.RegName IR.Value

propogateValues :: IR.Procedure -> Either String IR.Procedure
propogateValues (IR.Procedure blocks) = Right $ IR.Procedure $ propogateValuesBlocks blocks

propogateValuesBlocks :: [IR.Block] -> [IR.Block]
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
          \(instrs, lvs) instr -> let (instrs', lvs') = propogateValuesInstruction instr lvs in (instrs ++ instrs', lvs')
        ) ([], lastValues) instructions
  in (IR.Block label instructions', lastValues')

propogateValuesInstruction :: IR.Instruction -> LastValues -> ([IR.Instruction], LastValues)

propogateValuesInstruction (IR.Ret (Just val)) lastValues =
  ([IR.Ret $ Just $ optimizeValue val lastValues], lastValues)

propogateValuesInstruction (IR.Set (IR.Register reg) val) lastValues =
  -- Set will hopefully always have the effect of making the new register equal an old, different value
  ([], appendLastValue reg (optimizeValue val lastValues) lastValues)

propogateValuesInstruction (IR.BinOp op (IR.Register dest) src1 src2) lastValues =
  let newValA = optimizeValue src1 lastValues
      newValB = optimizeValue src2 lastValues
      newValue = evaluateBinOp op newValA newValB
  in case newValue of
    Just v -> ([], appendLastValue dest v lastValues)
    Nothing -> ([IR.BinOp op (IR.Register dest) newValA newValB], lastValues)

propogateValuesInstruction (IR.Phi reg pairs) lastValues =
  let newPairs = map (\(lr, v) -> (lr, optimizeValue v lastValues)) pairs
      values = map snd newPairs
      uniqueValues = Set.fromList values
  in case Set.size uniqueValues of
    0 -> error "Not possible to obtain value for phi"
    1 -> ([], appendLastValue reg (Set.elemAt 0 uniqueValues) lastValues)
    _ -> ([IR.Phi reg newPairs], lastValues)

propogateValuesInstruction (IR.JmpIf val refA refB) lastValues =
  let optValue = optimizeValue val lastValues in case optValue of
    IR.Immediate v -> ([IR.Jmp (if v == 0 then refB else refA)], lastValues)
    _ -> ([IR.JmpIf optValue refA refB], lastValues)

propogateValuesInstruction i lv = ([i], lv)

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
resolveValue :: IR.Value -> LastValues -> Maybe IR.Value
resolveValue (IR.Register v) lastValues = Map.lookup v lastValues
resolveValue (IR.Immediate i) _ = Just $ IR.Immediate i
resolveValue _ _ = Nothing

optimizeValue :: IR.Value -> LastValues -> IR.Value
optimizeValue val lastValues = fromMaybe val (resolveValue val lastValues)

appendLastValue :: IR.RegName -> IR.Value -> LastValues -> LastValues
appendLastValue reg val lastValues = Map.union lastValues (Map.singleton reg val)