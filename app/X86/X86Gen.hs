module X86.X86Gen where

import X86.X86Asm as Asm
import IR
import qualified Data.Map as Map

generateAsm :: IR.Module -> Either String X86Module
generateAsm (Module procedures fieldTable symbolTable) = Right $ X86Module (generateProcedures procedures ++ generateSymbols symbolTable)

generateProcedures :: [Procedure] -> [Section]
generateProcedures = map generateProcedure

-- TODO: We'll need to be able to merge sections with the same name
generateProcedure :: Procedure -> Section
generateProcedure (Procedure blocks) = Section "text" (map generateBlock blocks)

generateBlock :: Block -> LabelledBlock
generateBlock (Block (IR.LabelRef labelName) instructions) = LabelledBlock (Asm.Label labelName) (concatMap generateInstructions instructions)

generateInstructions :: Instruction -> [Instr]
generateInstructions (Ret value) = [Mov (Asm.Register EAX) (generateOperand value)]
generateInstructions (Set dest src) = [Mov (generateOperand dest) (generateOperand src)]

generateSymbols :: SymbolTable -> [Section]
generateSymbols (SymbolTable symbolMap _) = [Section "data" (map generateSymbol (Map.toList symbolMap))]

generateSymbol :: (String, IR.Literal) -> LabelledBlock
generateSymbol (name, lit) = LabelledBlock (Asm.Label name) [Db (generateLiteral lit)]

generateLiteral :: IR.Literal -> Asm.Literal
generateLiteral (IR.StringLiteral str) = Asm.StringLiteral str
generateLiteral (IR.IntLiteral int) = Asm.IntLiteral int

generateOperand :: IR.Value -> Operand
generateOperand (IR.Register _) = Asm.Register EAX -- TODO: Register allocation
generateOperand (IR.Immediate imm) = Asm.Immediate imm
generateOperand (IR.LabelReference (IR.LabelRef labelName)) = Asm.LabelRef (Asm.Label labelName)
generateOperand (IR.SymbolReference symbolName) = Asm.LabelRef (Asm.Label symbolName)
-- TODO: Labels and symbols should probably differ?