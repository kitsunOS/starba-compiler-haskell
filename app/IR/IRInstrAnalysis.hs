module IR.IRInstrAnalysis (defsV, usesV, successors, earlyExit) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified IR.IR as IR

defsV :: IR.Instruction -> [IR.Value]
defsV (IR.Ret _) = []
defsV (IR.Set value _) = [value]
defsV (IR.BinOp _ value1 _ _) = [value1]
defsV (IR.Phi value _) = [IR.Register value]
defsV (IR.Jmp _) = []
defsV (IR.JmpIf {}) = []

usesV :: IR.Instruction -> [IR.Value]
usesV (IR.Ret (Just value)) = [value]
usesV (IR.Ret _) = []
usesV (IR.Set _ value) = [value]
usesV (IR.BinOp _ _ value2 value3) = [value2, value3]
usesV (IR.Phi _ pairs) = map snd pairs
usesV (IR.Jmp _) = []
usesV (IR.JmpIf value _ _) = [value]

successors :: IR.Instruction -> [IR.LabelRef]
successors (IR.Jmp label) = [label]
successors (IR.JmpIf _ trueLabel falseLabel) = [trueLabel, falseLabel]
successors _ = []

earlyExit :: IR.Instruction -> Bool
earlyExit (IR.Ret _) = True
earlyExit (IR.Jmp _) = True
earlyExit _ = False

usedRegs :: IR.Block -> (Set.Set IR.Value, Set.Set IR.Value)
usedRegs block = foldl usedRegs' (Set.empty, Set.empty) (reverse (IR.blockInstructions block))
  where
    usedRegs' :: (Set.Set IR.Value, Set.Set IR.Value) -> IR.Instruction -> (Set.Set IR.Value, Set.Set IR.Value)
    usedRegs' (usedAcc, defAcc) inst =
      let used = Set.fromList (usesV inst)
          defs = Set.fromList (defsV inst)
      in (Set.difference (Set.union usedAcc used) defs, Set.union defAcc defs)