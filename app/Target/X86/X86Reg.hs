module Target.X86.X86Reg (intLive, regCompat) where

import qualified Target.X86.X86Asm as Asm
import qualified Backend.IR.IR as IR

intLive :: IR.Instruction -> [IR.RegEntry Asm.Register32]
intLive (IR.Ret _) = []
intLive (IR.Set _ _) = []
intLive (IR.BinOp IR.Mul _ _ _) = [IR.Physical Asm.EAX]
intLive (IR.BinOp IR.Div _ _ _) = [IR.Physical Asm.EAX, IR.Physical Asm.EDX]
intLive (IR.BinOp {}) = []
intLive (IR.Phi _ _) = []
intLive (IR.Jmp _) = []
intLive (IR.JmpIf {}) = []

regCompat :: IR.RegName -> IR.Instruction -> [Asm.Register32]
regCompat a (IR.BinOp t (IR.Register b) _ _) = comparisonCheck b a t
regCompat _ _ = defaultAllowedRegs

defaultAllowedRegs :: [Asm.Register32]
defaultAllowedRegs = [Asm.EAX, Asm.EBX, Asm.ECX, Asm.EDX, Asm.ESI, Asm.EDI]

comparisonAllowedRegs :: [Asm.Register32]
comparisonAllowedRegs = [Asm.EAX, Asm.EBX, Asm.ECX, Asm.EDX]

comparisonCheck :: IR.RegName -> IR.RegName -> IR.BinOpType -> [Asm.Register32]
comparisonCheck r1 r2 op =
  if op `elem` [IR.Eq, IR.Ne, IR.Lt, IR.Gt, IR.Le, IR.Ge] && r1 == r2
  then comparisonAllowedRegs
  else defaultAllowedRegs