module IR.IRInstrAnalysis (defs, uses, defsV, usesV, successors) where

import qualified Data.Set as Set
import qualified Data.Map as Map

import qualified IR.IR as IR

defs :: IR.Instruction -> [IR.RegName]
defs (IR.Ret _) = []
defs (IR.Set (IR.Register reg) _) = [reg]
defs (IR.Set _ _) = []
defs (IR.BinOp _ (IR.Register reg) _ _) = [reg]
defs (IR.BinOp {}) = []
defs (IR.Phi reg pairs) = error "Phi instruction should be eliminated by now"
defs (IR.Jmp _) = []
defs (IR.JmpIf {}) = []

uses :: IR.Instruction -> [IR.RegName]
uses (IR.Ret (Just (IR.Register reg))) = [reg]
uses (IR.Ret _) = []
uses (IR.Set _ (IR.Register reg)) = [reg]
uses (IR.Set _ _) = []
uses (IR.BinOp _ _ (IR.Register reg1) (IR.Register reg2)) = [reg1, reg2]
uses (IR.BinOp _ _ (IR.Register reg) _) = [reg]
uses (IR.BinOp _ _ _ (IR.Register reg)) = [reg]
uses (IR.BinOp {}) = []
uses (IR.Phi _ pairs) = error "Phi instruction should be eliminated by now"
uses (IR.Jmp _) = []
uses (IR.JmpIf (IR.Register reg) _ _) = [reg]
uses (IR.JmpIf {}) = []

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

usedRegs :: IR.Block -> (Set.Set IR.Value, Set.Set IR.Value)
usedRegs block = foldl usedRegs' (Set.empty, Set.empty) (reverse (IR.blockInstructions block))
  where
    usedRegs' :: (Set.Set IR.Value, Set.Set IR.Value) -> IR.Instruction -> (Set.Set IR.Value, Set.Set IR.Value)
    usedRegs' (usedAcc, defAcc) inst =
      let used = Set.fromList (usesV inst)
          defs = Set.fromList (defsV inst)
      in (Set.difference (Set.union usedAcc used) defs, Set.union defAcc defs)