module X86.X86Nasm (toNasmStr) where

import X86.X86Asm
import Numeric (showHex)
import Data.List (intercalate)
import qualified Register

toNasmStr :: X86Module -> String
toNasmStr (X86Module sections) = intercalate "\n\n" $ map sectionToNasm sections

sectionToNasm :: Section -> String
sectionToNasm (Section name blocks) = intercalate "\n" $ nameLine : map blockToNasm blocks
  where
    nameLine = "section ." ++ name

blockToNasm :: LabelledBlock -> String
blockToNasm (LabelledBlock (Label label) instrs global) =
  intercalate "\n" $ globalLine : labelLine : map instrToNasm instrs
  where
    labelLine = label ++ ":"
    globalLine = if global then "global " ++ label else ""

instrToNasm :: Instr -> String
instrToNasm (Mov dest src) = "mov " ++ operandToNasm dest ++ ", " ++ operandToNasm src
instrToNasm (Db lit) = "db " ++ literalToNasm lit
instrToNasm (Add dest src) = "add " ++ operandToNasm dest ++ ", " ++ operandToNasm src
instrToNasm (Sub dest src) = "sub " ++ operandToNasm dest ++ ", " ++ operandToNasm src
instrToNasm (Mul src) = "mul " ++ operandToNasm src
instrToNasm (Div divisor) = "div " ++ operandToNasm divisor
instrToNasm (Cmp dest src) = "cmp " ++ operandToNasm dest ++ ", " ++ operandToNasm src
instrToNasm (Push reg) = "push " ++ registerToNasm reg
instrToNasm (Pop reg) = "pop " ++ registerToNasm reg
instrToNasm (Jmp (Label label)) = "jmp " ++ label
instrToNasm (Je (Label label)) = "je " ++ label
instrToNasm (Sete reg) = "sete " ++ registerToNasm8 reg
instrToNasm (Setne reg) = "setne " ++ registerToNasm8 reg
instrToNasm (Setl reg) = "setl " ++ registerToNasm8 reg
instrToNasm (Setle reg) = "setle " ++ registerToNasm8 reg
instrToNasm (Setg reg) = "setg " ++ registerToNasm8 reg
instrToNasm (Setge reg) = "setge " ++ registerToNasm8 reg
instrToNasm (Movzx dest src) = "movzx " ++ registerToNasm dest ++ ", " ++ registerToNasm8 src
instrToNasm (Neg reg) = "neg " ++ registerToNasm reg
instrToNasm Ret = "ret"

operandToNasm :: Operand -> String
operandToNasm (Register reg) = registerToNasm reg
operandToNasm (Immediate imm) = show imm
operandToNasm (Memory mem) = concat ["[", "0x", showHex mem "", "]"]
operandToNasm (LabelRef (Label label)) = label

registerToNasm :: Register32 -> String
registerToNasm = Register.formatName

registerToNasm8 :: Register8 -> String
registerToNasm8 = Register.formatName

literalToNasm :: Literal -> String
literalToNasm (StringLiteral str) = show (length str) ++ ", \"" ++ str ++ "\""