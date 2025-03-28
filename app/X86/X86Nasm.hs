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
instrToNasm (Push reg) = "push " ++ registerToNasm reg
instrToNasm (Pop reg) = "pop " ++ registerToNasm reg
instrToNasm Ret = "ret"

operandToNasm :: Operand -> String
operandToNasm (Register reg) = registerToNasm reg
operandToNasm (Immediate imm) = show imm
operandToNasm (Memory mem) = concat ["[", "0x", showHex mem "", "]"]
operandToNasm (LabelRef (Label label)) = label

registerToNasm :: Register32 -> String
registerToNasm = Register.formatName

literalToNasm :: Literal -> String
literalToNasm (StringLiteral str) = show (length str) ++ ", \"" ++ str ++ "\""