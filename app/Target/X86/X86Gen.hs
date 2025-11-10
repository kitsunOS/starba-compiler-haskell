module Target.X86.X86Gen where

import qualified Data.Map as Map

import qualified Target.X86.X86Asm as Asm
import qualified Backend.IR.IR as IR
import qualified Backend.Reg.RegAlloc as RegAlloc

newtype GenerationContext = GenerationContext {
  gAllocation :: RegAlloc.Allocation Asm.Register32
}

generateAsm :: GenerationContext -> IR.Module -> Either String Asm.X86Module
generateAsm ctx (IR.Module procedures fieldTable symbolTable) = Right $ Asm.X86Module (generateProcedures ctx procedures ++ generateSymbols symbolTable)

generateProcedures :: GenerationContext -> [IR.Procedure] -> [Asm.Section]
generateProcedures ctx = map (generateProcedure ctx)

-- TODO: We'll need to be able to merge sections with the same name
generateProcedure :: GenerationContext -> IR.Procedure -> Asm.Section
generateProcedure ctx (IR.Procedure blocks) = Asm.Section "text"
  (generateBlock ctx True (head blocks) : map (generateBlock ctx False) (tail blocks))

generateBlock :: GenerationContext -> Bool -> IR.Block -> Asm.LabelledBlock
generateBlock ctx global (IR.Block (IR.LabelRef labelName) instructions) = Asm.LabelledBlock {
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
  Asm.Jmp _ -> [ instr ]
  _ -> instr : safeExit rest

-- TODO: Is the validOperand a good approach? Or should this be handled elsewhere?
generateInstructions :: GenerationContext -> IR.Instruction -> [Asm.Instr]
generateInstructions ctx (IR.Ret (Just value)) = [
  Asm.Mov (Asm.Register Asm.EAX) (generateOperand ctx value),
  Asm.Ret
  ]
generateInstructions ctx (IR.Ret Nothing) = [Asm.Ret]
generateInstructions ctx (IR.Set dest src) = [Asm.Mov rDest rSrc1 | validOperand ctx dest]
  where
    rDest = generateOperand ctx dest
    rSrc1 = generateOperand ctx src
generateInstructions ctx (IR.BinOp IR.Add dest src1 src2)
  | not (validOperand ctx dest) = []
  | rDest == rSrc1 = [
    Asm.Add rDest rSrc2]
  | rDest == rSrc2 = [
    Asm.Add rDest rSrc1]
  | otherwise = [
    Asm.Mov rDest rSrc2,
    Asm.Add rDest rSrc1]
  where
    rDest = generateOperand ctx dest
    rSrc1 = generateOperand ctx src1
    rSrc2 = generateOperand ctx src2
generateInstructions ctx (IR.BinOp IR.Sub dest src1 src2)
  | not (validOperand ctx dest) = []
  | rDest == rSrc1 = [
    Asm.Sub rDest rSrc2]
  | rDest == rSrc2 = [
    Asm.Sub rDest rSrc1,
    Asm.Neg $ requireReg32 rDest
    ]
  | otherwise = [
    Asm.Mov rDest rSrc1,
    Asm.Sub rDest rSrc2]
  where
    rDest = generateOperand ctx dest
    rSrc1 = generateOperand ctx src1
    rSrc2 = generateOperand ctx src2
generateInstructions ctx (IR.BinOp IR.Mul dest src1 src2) = if not (validOperand ctx dest) then [] else [
  -- TODO: Does EDX need to be 0?
  Asm.Mov (Asm.Register Asm.EAX) rSrc1,
  Asm.Mul rSrc2,
  Asm.Mov rDest (Asm.Register Asm.EAX)
  ]
  where
    rDest = generateOperand ctx dest
    rSrc1 = generateOperand ctx src1
    rSrc2 = generateOperand ctx src2
generateInstructions ctx (IR.BinOp IR.Div dest src1 src2) = if not (validOperand ctx dest) then [] else [
  Asm.Mov (Asm.Register Asm.EDX) (Asm.Immediate 0),
  Asm.Mov (Asm.Register Asm.EAX) rSrc1,
  Asm.Div rSrc2,
  Asm.Mov rDest (Asm.Register Asm.EAX)
  ]
  where
    rDest = generateOperand ctx dest
    rSrc1 = generateOperand ctx src1
    rSrc2 = generateOperand ctx src2
generateInstructions ctx (IR.BinOp op dest src1 src2) = if not (validOperand ctx dest) then [] else
  [
    Asm.Cmp rSrc1 rSrc2,
    case op of
      IR.Eq -> Asm.Sete rDest8
      IR.Lt -> Asm.Setl rDest8
      IR.Le -> Asm.Setle rDest8
      IR.Gt -> Asm.Setg rDest8
      IR.Ge -> Asm.Setge rDest8
      IR.Ne -> Asm.Setne rDest8,
    Asm.Movzx rDest (Asm.reg32To8 rDest)
    ]
  where
    rDest = requireReg32 (generateOperand ctx dest)
    rDest8 = Asm.reg32To8 rDest
    rSrc1 = generateOperand ctx src1
    rSrc2 = generateOperand ctx src2
generateInstructions ctx (IR.Jmp (IR.LabelRef label)) = [Asm.Jmp (Asm.Label label)]
generateInstructions ctx (IR.JmpIf (IR.Immediate v) (IR.LabelRef trueLabel) (IR.LabelRef falseLabel)) = [
  Asm.Jmp $ Asm.Label (if v == 0 then falseLabel else trueLabel)
  ]
generateInstructions ctx (IR.JmpIf cond (IR.LabelRef trueLabel) (IR.LabelRef falseLabel)) = [
  Asm.Cmp (generateOperand ctx cond) (Asm.Immediate 0),
  Asm.Je (Asm.Label falseLabel),
  Asm.Jmp (Asm.Label trueLabel)
  ]

generateSymbols :: IR.SymbolTable -> [Asm.Section]
generateSymbols (IR.SymbolTable symbolMap _) = [Asm.Section "data" (map generateSymbol (Map.toList symbolMap))]

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

validOperand :: GenerationContext -> IR.Value -> Bool
validOperand ctx (IR.Register regName) = Map.member regName (RegAlloc.allocatedRegisters (gAllocation ctx))
validOperand _ _ = True

requireReg32 :: Asm.Operand -> Asm.Register32
requireReg32 (Asm.Register reg) = reg
requireReg32 _ = error "Expected a 32-bit register"