module IRGen (compileModule) where

import AST
import IR
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Set as Set

-- Data types
data IRGenD = IRGen {
  irgBlocks :: [Block],
  irgSymbolTable :: SymbolTable,
  irgNextRegister :: RegName,
  irgNextSymbolName :: String,
  irgActiveRegisters :: Map.Map VarRef (Map.Map LabelRef RegName),
  irgActiveLabel :: LabelRef,
  irgAllLabels :: Set.Set LabelRef
}
type IRGenM = StateT IRGenD (Except String)
defaultState :: IRGenD
defaultState = IRGen {
  irgBlocks = [],
  irgSymbolTable = SymbolTable Map.empty Map.empty,
  irgNextRegister = RegName "a" 0,
  irgNextSymbolName = "a",
  irgActiveRegisters = Map.empty,
  irgActiveLabel = LabelRef "entry",
  irgAllLabels = Set.empty
}

data DeclarationContribution = DeclarationContribution {
  declProcedures :: [Procedure],
  declSymbolTable :: SymbolTable,
  declFieldTable :: FieldTable
}

-- IRGen helpers
nextLabel :: String -> IRGenM LabelRef
nextLabel label = nextLabelPostfix label ""
  where
    nextLabelPostfix :: String -> String -> IRGenM LabelRef
    nextLabelPostfix label postFix = do
      let label' = label ++ case postFix of
            "" -> ""
            _  -> "_" ++ postFix
      allLabels <- gets irgAllLabels
      if Set.member (LabelRef label') allLabels then
        nextLabelPostfix label (incrementName postFix)
      else (do
        modify $ \s -> s { irgAllLabels = Set.insert (LabelRef label') (irgAllLabels s) }
        return $ LabelRef label')

getNextRegister :: IRGenM RegName
getNextRegister = do
  regName <- gets irgNextRegister
  modify $ \s -> s { irgNextRegister = nextRegName regName }
  return regName

addInstruction :: Instruction -> IRGenM ()
addInstruction instruction = modify $ \s -> s { irgBlocks = addInstructionToBlock instruction (irgBlocks s) (irgActiveLabel s) }

addInstructionToBlock :: Instruction -> [Block] -> LabelRef -> [Block]
addInstructionToBlock instruction [] label = [Block label [instruction]]
addInstructionToBlock instruction (Block label instructions : blocks) activeLabel
  | label == activeLabel = Block label (instruction : instructions) : blocks
  | otherwise = Block label instructions : addInstructionToBlock instruction blocks activeLabel

startBlock :: LabelRef -> LabelRef -> IRGenM ()
startBlock label inheritedLabel = do
  modify $ \s -> s { irgBlocks = Block label [] : irgBlocks s, irgActiveLabel = label }
  modify $ \s -> s { irgActiveRegisters = inheritVars label (irgActiveRegisters s) inheritedLabel }
  return ()
  where
    inheritVars :: LabelRef -> Map.Map VarRef (Map.Map LabelRef RegName) -> LabelRef -> Map.Map VarRef (Map.Map LabelRef RegName)
    inheritVars newLabel activeRegisters inheritedLabel =
      Map.mapWithKey (\varName regMap -> Map.insert newLabel (regMap Map.! inheritedLabel) regMap) activeRegisters

allocateSymbol :: String -> IRGenM String
allocateSymbol value = do
  symbolName <- gets irgNextSymbolName
  symbolTable <- gets irgSymbolTable
  let lit = IR.StringLiteral value
      existingSymbol = Map.lookup lit (reverseMap symbolTable)
  case existingSymbol of
    Just name -> return name
    Nothing -> do
      let symbolMap' = Map.insert symbolName lit (symbolMap symbolTable)
          reverseMap' = Map.insert lit symbolName (reverseMap symbolTable)
          symbolTable' = SymbolTable symbolMap' reverseMap'
      modify $ \s -> s { irgSymbolTable = symbolTable', irgNextSymbolName = incrementName symbolName }
      return symbolName

-- Small helpers
mergeSymbolTables :: [SymbolTable] -> SymbolTable
mergeSymbolTables = foldl mergeSymbolTable (SymbolTable Map.empty Map.empty)

mergeSymbolTable :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTable (SymbolTable symbolMap1 reverseMap1) (SymbolTable symbolMap2 reverseMap2) =
  SymbolTable (Map.union symbolMap1 symbolMap2) (Map.union reverseMap1 reverseMap2)

mergeFieldTables :: [FieldTable] -> FieldTable
mergeFieldTables = foldl mergeFieldTable (FieldTable Map.empty)

mergeFieldTable :: FieldTable -> FieldTable -> FieldTable
mergeFieldTable (FieldTable fieldMap1) (FieldTable fieldMap2) =
  FieldTable (Map.union fieldMap1 fieldMap2)

nextRegName :: RegName -> RegName
nextRegName (RegName prefix num) =
  RegName (incrementName prefix) 0

incrementName :: String -> String
incrementName = reverse . increment . reverse
  where
    increment [] = ['a']
    increment ('z':xs) = 'a' : increment xs
    increment (x:xs) = succ x : xs

-- Compilation
compileModule :: AST.Module -> Either String IR.Module
compileModule (AST.Module decls) = do
  let compile = mapM compileDeclaration decls
  case runExcept (evalStateT compile defaultState) of
    Left err -> Left err
    Right contributions ->
      let procedures = concatMap declProcedures contributions
          symbolTable = mergeSymbolTables $ map declSymbolTable contributions
          fieldTable = mergeFieldTables $ map declFieldTable contributions
      in Right $ IR.Module procedures fieldTable symbolTable

compileDeclaration :: AST.Declaration -> IRGenM DeclarationContribution
compileDeclaration (AST.Declaration name visibility value) =
  case value of
    AST.VarDeclarationValue varDef -> compileVarDeclaration name visibility varDef
    AST.FuncDeclarationValue funcDef -> compileFuncDeclaration name visibility funcDef
    AST.EnumDeclarationValue enumDef -> compileEnumDeclaration name visibility enumDef

compileVarDeclaration :: String -> AST.Visibility -> AST.VariableDefinition -> IRGenM DeclarationContribution
compileVarDeclaration name visibility (AST.VariableDefinition typ initializer) =
  let
    -- TODO: Evaluate the actual initializer
    fieldTable = FieldTable $ Map.singleton name (IntLiteral 0)
  in
    return $ DeclarationContribution [] (SymbolTable Map.empty Map.empty) fieldTable

compileEnumDeclaration :: String -> AST.Visibility -> AST.EnumDefinition -> IRGenM DeclarationContribution
compileEnumDeclaration name visibility (AST.EnumDefinition _ values members) =
  -- TODO: Implement
  return $ DeclarationContribution [] (SymbolTable Map.empty Map.empty) (FieldTable Map.empty)

compileFuncDeclaration :: String -> AST.Visibility -> AST.FunctionDefinition -> IRGenM DeclarationContribution
compileFuncDeclaration _ _ (AST.FunctionDefinition _ _ Nothing) = throwError "Function declaration without body not yet supported"
compileFuncDeclaration _ _ (AST.FunctionDefinition _ _ (Just body)) = do
  compileFuncBody body
  let reverseBlock (Block label instructions) = Block label (reverse instructions)
  blocks <- gets (map reverseBlock . reverse . irgBlocks)
  symbolTable <- gets irgSymbolTable
  return $ DeclarationContribution [Procedure blocks] symbolTable (FieldTable Map.empty)

compileFuncBody :: AST.FunctionBody -> IRGenM ()
compileFuncBody (AST.FunctionBody stmts) = mapM_ compileStatement stmts

compileStatement :: AST.Statement -> IRGenM ()
compileStatement (AST.InnerDecl innerDecl) = compileInnerDeclaration innerDecl
compileStatement (AST.Return expr) = compileReturn expr

compileInnerDeclaration :: AST.InnerDeclaration -> IRGenM ()
compileInnerDeclaration (AST.InnerDeclaration name (AST.InnerVarDeclarationValue varDef)) = compileInnerVarDeclaration name varDef

compileInnerVarDeclaration :: String -> AST.VariableDefinition -> IRGenM ()
compileInnerVarDeclaration name (AST.VariableDefinition typ Nothing) = throwError "Inner variable declaration without initializer not yet supported"
compileInnerVarDeclaration name (AST.VariableDefinition typ (Just initializer)) = do
  regName <- compileExpression initializer
  originalActiveRegisters <- gets irgActiveRegisters
  activeLabel <- gets irgActiveLabel
  let newActiveRegisters = Map.alter updateInnerMap name originalActiveRegisters
        where
          updateInnerMap Nothing = Just $ Map.singleton activeLabel regName
          updateInnerMap (Just labelMap) = Just $ Map.insert activeLabel regName labelMap
  modify $ \s -> s { irgActiveRegisters = newActiveRegisters }

compileReturn :: Maybe AST.Expression -> IRGenM ()
compileReturn Nothing = addInstruction $ Ret Nothing
compileReturn (Just expr) = do
  regName <- compileExpression expr
  addInstruction $ Ret (Just (Register regName))

compileExpression :: AST.Expression -> IRGenM RegName
compileExpression (NumberLiteral num) = do
  regName <- getNextRegister
  addInstruction $ Set (Register regName) (Immediate num)
  return regName
compileExpression (AST.StringLiteral str) = do
  symbolName <- allocateSymbol str
  regName <- getNextRegister
  addInstruction $ Set (Register regName) (SymbolReference symbolName)
  return regName
compileExpression (AST.BinOp op left right) = do
  leftReg <- compileExpression left
  rightReg <- compileExpression right
  regName <- getNextRegister
  addInstruction $ IR.BinOp (irBinOp op) (Register regName) (Register leftReg) (Register rightReg)
  return regName
compileExpression (Variable var) = do
  activeRegisters <- gets irgActiveRegisters
  activeLabel <- gets irgActiveLabel
  case Map.lookup var activeRegisters >>= Map.lookup activeLabel of
    Just regName -> return regName
    Nothing -> throwError $ "Variable " ++ var ++ " not in scope"
compileExpression (AST.Ternary cond trueExpr falseExpr) = do
  condReg <- compileExpression cond
  trueLabel <- nextLabel "ternary_true"
  falseLabel <- nextLabel "ternary_false"
  endLabel <- nextLabel "ternary_end"
  currentLabel <- gets irgActiveLabel
  addInstruction $ IR.JmpIf (Register condReg) trueLabel falseLabel
  startBlock trueLabel currentLabel
  trueReg <- compileExpression trueExpr
  addInstruction $ IR.Jmp endLabel
  startBlock falseLabel currentLabel
  falseReg <- compileExpression falseExpr
  addInstruction $ IR.Jmp endLabel
  startBlock endLabel currentLabel
  outReg <- getNextRegister
  addInstruction $ IR.Phi outReg [(trueLabel, trueReg), (falseLabel, falseReg)]
  return outReg


irBinOp :: String -> BinOpType
irBinOp "+" = Add
irBinOp "-" = Sub
irBinOp "*" = Mul
irBinOp "/" = Div
irBinOp "==" = Eq
irBinOp _ = error "Unknown binary operator"