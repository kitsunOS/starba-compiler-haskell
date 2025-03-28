module X86.X86Gen where

import qualified X86.X86Asm as Asm
import IR
import qualified Data.Map as Map
import qualified RegAlloc

newtype GenerationContext = GenerationContext {
  gAllocation :: RegAlloc.Allocation Asm.Register32
}

generateAsm :: GenerationContext -> IR.Module -> Either String Asm.X86Module
generateAsm ctx (Module procedures fieldTable symbolTable) = Right $ Asm.X86Module (generateProcedures ctx procedures ++ generateSymbols symbolTable)

generateProcedures :: GenerationContext -> [Procedure] -> [Asm.Section]
generateProcedures ctx = map (generateProcedure ctx)

-- TODO: We'll need to be able to merge sections with the same name
generateProcedure :: GenerationContext -> Procedure -> Asm.Section
generateProcedure ctx (Procedure blocks) = Asm.Section "text"
  (generateBlock ctx True (head blocks) : map (generateBlock ctx False) (tail blocks))

generateBlock :: GenerationContext -> Bool -> Block -> Asm.LabelledBlock
generateBlock ctx global (Block (IR.LabelRef labelName) instructions) = Asm.LabelledBlock {
  Asm.blockLabel = Asm.Label labelName,
  Asm.blockInsts = if global then entryHeader ++ instructions' else instructions',
  Asm.blockGlobal = global
}
  where
    instructions' = safeExit $ concatMap (generateInstructions ctx) instructions
    entryHeader = [
      Asm.Push Asm.EBP,
      Asm.Mov (Asm.Register Asm.EBP) (Asm.Register Asm.ESP),
      Asm.Push Asm.EBX]

safeExitFooter :: [Asm.Instr]
safeExitFooter = [
  Asm.Pop Asm.EBX,
  Asm.Mov (Asm.Register Asm.ESP) (Asm.Register Asm.EBP),
  Asm.Pop Asm.EBP,
  Asm.Ret]

safeExit :: [Asm.Instr] -> [Asm.Instr]
safeExit [] = safeExitFooter
safeExit (instr : rest) = case instr of
  Asm.Ret -> safeExitFooter ++ if null rest then [] else safeExit rest
  _ -> instr : safeExit rest
generateInstructions :: GenerationContext -> Instruction -> [Asm.Instr]
generateInstructions ctx (Ret (Just value)) = [
  Asm.Mov (Asm.Register Asm.EAX) (generateOperand ctx value),
  Asm.Ret
  ]
generateInstructions ctx (Ret Nothing) = [Asm.Ret]
generateInstructions ctx (Set dest src) = [Asm.Mov (generateOperand ctx dest) (generateOperand ctx src)]
generateInstructions ctx (BinOp IR.Add dest src1 src2)
  | rDest == rSrc1 = [
    Asm.Add (generateOperand ctx dest) (generateOperand ctx src2)]
  | rDest == rSrc2 = [
    Asm.Add (generateOperand ctx dest) (generateOperand ctx src1)]
  | otherwise = [
    Asm.Mov (generateOperand ctx dest) (generateOperand ctx src2),
    Asm.Add (generateOperand ctx dest) (generateOperand ctx src1)]
  where
    rDest = generateOperand ctx dest
    rSrc1 = generateOperand ctx src1
    rSrc2 = generateOperand ctx src2
generateInstructions ctx (BinOp IR.Sub dest src1 src2)
  | rDest == rSrc1 = [
    Asm.Sub (generateOperand ctx dest) (generateOperand ctx src2)]
  | rDest == rSrc2 = [
    Asm.Sub (generateOperand ctx dest) (generateOperand ctx src1)]
  | otherwise = [
    Asm.Mov (generateOperand ctx dest) (generateOperand ctx src2),
    Asm.Sub (generateOperand ctx dest) (generateOperand ctx src1)]
  where
    rDest = generateOperand ctx dest
    rSrc1 = generateOperand ctx src1
    rSrc2 = generateOperand ctx src2
generateInstructions ctx (BinOp IR.Mul dest src1 src2) = [
  Asm.Mov (Asm.Register Asm.EAX) (generateOperand ctx src1),
  Asm.Mul (generateOperand ctx src2),
  Asm.Mov (generateOperand ctx dest) (Asm.Register Asm.EAX)
  ]
generateInstructions ctx (BinOp IR.Div dest src1 src2) = [
  Asm.Mov (Asm.Register Asm.EDX) (Asm.Immediate 0),
  Asm.Mov (Asm.Register Asm.EAX) (generateOperand ctx src1),
  Asm.Div (generateOperand ctx src2),
  Asm.Mov (generateOperand ctx dest) (Asm.Register Asm.EAX)
  ]

generateSymbols :: SymbolTable -> [Asm.Section]
generateSymbols (SymbolTable symbolMap _) = [Asm.Section "data" (map generateSymbol (Map.toList symbolMap))]

generateSymbol :: (String, IR.Literal) -> Asm.LabelledBlock
generateSymbol (name, lit) = Asm.LabelledBlock {
  Asm.blockLabel = Asm.Label name,
  Asm.blockInsts = [Asm.Db (generateLiteral lit)],
  Asm.blockGlobal = False
}

generateLiteral :: IR.Literal -> Asm.Literal
generateLiteral (IR.StringLiteral str) = Asm.StringLiteral str
generateLiteral (IR.IntLiteral int) = Asm.IntLiteral int

generateOperand :: GenerationContext -> IR.Value -> Asm.Operand
-- TODO: This doesn't work properly with registers in dead code
generateOperand ctx (IR.Register regName) = Asm.Register $ RegAlloc.allocatedRegisters (gAllocation ctx) Map.! regName
generateOperand _ (IR.Immediate imm) = Asm.Immediate imm
generateOperand _ (IR.LabelReference (IR.LabelRef labelName)) = Asm.LabelRef (Asm.Label labelName)
generateOperand _ (IR.SymbolReference symbolName) = Asm.LabelRef (Asm.Label symbolName)
-- TODO: Labels and symbols should probably differ?