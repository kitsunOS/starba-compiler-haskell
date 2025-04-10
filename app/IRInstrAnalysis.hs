module IRInstrAnalysis where

import qualified IR

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

successors :: IR.Instruction -> [IR.LabelRef]
successors (IR.Jmp label) = [label]
successors (IR.JmpIf _ trueLabel falseLabel) = [trueLabel, falseLabel]
successors _ = []