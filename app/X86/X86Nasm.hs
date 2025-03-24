module X86.X86Nasm (toNasmStr) where

import X86.X86Asm
import Numeric (showHex)
import Data.List (intercalate)

toNasmStr :: X86Module -> String
toNasmStr (X86Module sections) = intercalate "\n\n" $ map sectionToNasm sections

sectionToNasm :: Section -> String
sectionToNasm (Section name blocks) = intercalate "\n" $ nameLine : map blockToNasm blocks
  where
    nameLine = "section ." ++ name

blockToNasm :: LabelledBlock -> String
blockToNasm (LabelledBlock (Label label) instrs) = intercalate "\n" $ labelLine : map instrToNasm instrs
  where
    labelLine = label ++ ":"

instrToNasm :: Instr -> String
instrToNasm (Mov dest src) = "mov " ++ operandToNasm dest ++ ", " ++ operandToNasm src
instrToNasm (Db lit) = "db " ++ literalToNasm lit

operandToNasm :: Operand -> String
operandToNasm (Register reg) = registerToNasm reg
operandToNasm (Immediate imm) = show imm
operandToNasm (Memory mem) = concat ["[", "0x", showHex mem "", "]"]
operandToNasm (LabelRef (Label label)) = label

registerToNasm :: Register32 -> String
registerToNasm EAX = "eax"
registerToNasm EBX = "ebx"
registerToNasm ECX = "ecx"
registerToNasm EDX = "edx"

literalToNasm :: Literal -> String
literalToNasm (StringLiteral str) = show (length str) ++ ", \"" ++ str ++ "\""