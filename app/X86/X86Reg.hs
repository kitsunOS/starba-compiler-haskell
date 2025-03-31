module X86.X86Reg where
import qualified X86.X86Asm as Asm
import qualified IR
import qualified Debug.Trace as Debug

intLive :: IR.Instruction -> [IR.RegEntry Asm.Register32]
intLive (IR.Ret _) = []
intLive (IR.Set _ _) = []
intLive (IR.BinOp IR.Mul _ _ _) = Debug.traceShowId [IR.Physical Asm.EAX]
intLive (IR.BinOp IR.Div _ _ _) = [IR.Physical Asm.EAX, IR.Physical Asm.EDX]
intLive (IR.BinOp {}) = []