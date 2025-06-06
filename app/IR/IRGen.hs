module IR.IRGen (compileModule) where

import qualified AST.AST as AST
import IR.IR (
  Module(..), Procedure(..), Block(..), Instruction(..), RegName(..),
  LabelRef(..), Value(..), SymbolTable(..), FieldTable(..), VarRef)
import qualified IR.IR as IR
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Set as Set
import qualified Data.Foldable
import Control.Arrow (ArrowLoop(loop))

-- Data types
type Sym = AST.Symbol

data IRGenD = IRGenD {
  irgBlocks :: [Block],
  irgSymbolTable :: SymbolTable,
  irgNextRegister :: RegName,
  irgNextSymbolName :: String,
  irgActiveLabel :: LabelRef,
  irgAllLabels :: Set.Set LabelRef
}
type IRGenM = StateT IRGenD (Except String)

defaultState :: IRGenD
defaultState = IRGenD {
  irgBlocks = [],
  irgSymbolTable = SymbolTable Map.empty Map.empty,
  irgNextRegister = RegName "a" 0,
  irgNextSymbolName = "a",
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
  return ()
  where
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
compileModule :: AST.Module Sym -> Either String IR.Module
compileModule (AST.Module decls) = do
  let compile = mapM compileDeclaration decls
  case runExcept (evalStateT compile defaultState) of
    Left err -> Left err
    Right contributions ->
      let procedures = concatMap declProcedures contributions
          symbolTable = mergeSymbolTables $ map declSymbolTable contributions
          fieldTable = mergeFieldTables $ map declFieldTable contributions
      in Right $ IR.Module procedures fieldTable symbolTable

compileDeclaration :: AST.Declaration Sym -> IRGenM DeclarationContribution
compileDeclaration (AST.Declaration name visibility value) =
  case value of
    AST.VarDeclarationValue varDef -> compileVarDeclaration name visibility varDef
    AST.FuncDeclarationValue funcDef -> compileFuncDeclaration name visibility funcDef
    AST.EnumDeclarationValue enumDef -> compileEnumDeclaration name visibility enumDef

compileVarDeclaration :: Sym -> AST.Visibility -> AST.VariableDefinition Sym -> IRGenM DeclarationContribution
compileVarDeclaration name visibility (AST.VariableDefinition typ initializer) =
  let
    -- TODO: Evaluate the actual initializer
    -- TODO: How to format field name?
    fieldTable = FieldTable $ Map.singleton (show name) (IR.IntLiteral 0)
  in
    return $ DeclarationContribution [] (SymbolTable Map.empty Map.empty) fieldTable

compileEnumDeclaration :: Sym -> AST.Visibility -> AST.EnumDefinition Sym -> IRGenM DeclarationContribution
compileEnumDeclaration name visibility (AST.EnumDefinition _ values members) =
  -- TODO: Implement
  return $ DeclarationContribution [] (SymbolTable Map.empty Map.empty) (FieldTable Map.empty)

compileFuncDeclaration :: Sym -> AST.Visibility -> AST.FunctionDefinition Sym -> IRGenM DeclarationContribution
compileFuncDeclaration _ _ (AST.FunctionDefinition _ _ Nothing) = throwError "Function declaration without body not yet supported"
compileFuncDeclaration _ _ (AST.FunctionDefinition _ _ (Just body)) = do
  compileFuncBody body
  let reverseBlock (Block label instructions) = Block label (reverse instructions)
  blocks <- gets (map reverseBlock . reverse . irgBlocks)
  symbolTable <- gets irgSymbolTable
  return $ DeclarationContribution [Procedure blocks] symbolTable (FieldTable Map.empty)

compileFuncBody :: AST.FunctionBody Sym -> IRGenM ()
compileFuncBody (AST.FunctionBody stmts) = mapM_ compileStatement stmts

compileStatement :: AST.Statement Sym -> IRGenM ()
compileStatement (AST.InnerDecl innerDecl) = compileInnerDeclaration innerDecl
compileStatement (AST.Return expr) = compileReturn expr
compileStatement (AST.Assignment varName expr) = void $ compileAssignment varName expr
compileStatement (AST.If cond trueBranch elseBranch) = compileIfStatement cond trueBranch elseBranch
compileStatement (AST.While cond body) = compileWhileStatement cond body
compileStatement (AST.For maybeInit maybeCond maybeStep body) = compileForStatement maybeInit maybeCond maybeStep body
compileStatement (AST.BlockBody stmts) = mapM_ compileStatement stmts

compileIfStatement :: AST.Expression Sym -> AST.Statement Sym -> Maybe (AST.Statement Sym) -> IRGenM ()
compileIfStatement cond trueBranch Nothing = do
  condVal <- compileExpression cond
  trueLabel <- nextLabel "if_true"
  endLabel <- nextLabel "if_end"
  currentLabel <- gets irgActiveLabel
  addInstruction $ IR.JmpIf condVal trueLabel endLabel
  startBlock trueLabel currentLabel
  compileStatement trueBranch
  addInstruction $ IR.Jmp endLabel
  startBlock endLabel currentLabel
compileIfStatement cond trueBranch (Just falseBranch) = do
  condVal <- compileExpression cond
  trueLabel <- nextLabel "if_true"
  falseLabel <- nextLabel "if_false"
  endLabel <- nextLabel "if_end"
  currentLabel <- gets irgActiveLabel
  addInstruction $ IR.JmpIf condVal trueLabel falseLabel
  startBlock trueLabel currentLabel
  compileStatement trueBranch
  addInstruction $ IR.Jmp endLabel
  startBlock falseLabel currentLabel
  compileStatement falseBranch
  addInstruction $ IR.Jmp endLabel
  startBlock endLabel currentLabel

compileWhileStatement :: AST.Expression Sym -> AST.Statement Sym -> IRGenM ()
compileWhileStatement cond body = do
  startLabel <- nextLabel "while_start"
  loopLabel <- nextLabel "while_loop"
  endLabel <- nextLabel "while_end"
  currentLabel <- gets irgActiveLabel
  addInstruction $ IR.Jmp startLabel
  startBlock startLabel currentLabel
  condVal <- compileExpression cond
  addInstruction $ IR.JmpIf condVal loopLabel endLabel
  startBlock loopLabel currentLabel
  compileStatement body
  addInstruction $ IR.Jmp startLabel
  startBlock endLabel currentLabel

compileForStatement :: Maybe (AST.InnerDeclaration Sym) -> Maybe (AST.Expression Sym) -> Maybe (AST.Expression Sym) -> AST.Statement Sym -> IRGenM ()
compileForStatement maybeInit maybeCond maybeStep body = do
  startLabel <- nextLabel "for_start"
  endLabel <- nextLabel "for_end"
  currentLabel <- gets irgActiveLabel
  addInstruction $ IR.Jmp startLabel
  startBlock startLabel currentLabel
  case maybeInit of
    Just (AST.InnerDeclaration name (AST.InnerVarDeclarationValue varDef)) -> compileInnerVarDeclaration name varDef
    Nothing -> return ()
  case maybeCond of
    Just cond -> do
      condVal <- compileExpression cond
      addInstruction $ IR.JmpIf condVal startLabel endLabel
    Nothing -> return ()
  compileStatement body
  -- TODO: Handle step expression
  addInstruction $ IR.Jmp startLabel
  startBlock endLabel currentLabel

compileInnerDeclaration :: AST.InnerDeclaration Sym -> IRGenM ()
compileInnerDeclaration (AST.InnerDeclaration name (AST.InnerVarDeclarationValue varDef)) = compileInnerVarDeclaration name varDef

compileInnerVarDeclaration :: Sym -> AST.VariableDefinition Sym -> IRGenM ()
compileInnerVarDeclaration name (AST.VariableDefinition typ Nothing) = throwError "Inner variable declaration without initializer not yet supported"
compileInnerVarDeclaration name (AST.VariableDefinition typ (Just initializer)) = do
  val <- compileExpression initializer
  addInstruction $ Set (VariableReference name) val

compileReturn :: Maybe (AST.Expression Sym) -> IRGenM ()
compileReturn Nothing = addInstruction $ Ret Nothing
compileReturn (Just expr) = do
  val <- compileExpression expr
  addInstruction $ Ret (Just val)

compileAssignment :: Sym -> AST.Expression Sym -> IRGenM IR.Value
compileAssignment varName expr = do
  val <- compileExpression expr
  addInstruction $ Set (VariableReference varName) val
  return val

compileExpression :: AST.Expression Sym -> IRGenM Value
compileExpression (AST.NumberLiteral num) = return $ IR.Immediate num
compileExpression (AST.StringLiteral str) = do
  symbolName <- allocateSymbol str
  return $ SymbolReference symbolName
compileExpression (AST.BinOp op left right) = do
  leftVal <- compileExpression left
  rightVal <- compileExpression right
  regName <- getNextRegister
  addInstruction $ IR.BinOp (irBinOp op) (Register regName) leftVal rightVal
  return $ IR.Register regName
compileExpression (AST.Variable varName) = return $ IR.VariableReference varName
compileExpression (AST.Ternary cond trueExpr falseExpr) = do
  condVal <- compileExpression cond
  trueLabel <- nextLabel "ternary_true"
  falseLabel <- nextLabel "ternary_false"
  endLabel <- nextLabel "ternary_end"
  currentLabel <- gets irgActiveLabel
  addInstruction $ IR.JmpIf condVal trueLabel falseLabel
  startBlock trueLabel currentLabel
  trueVal <- compileExpression trueExpr
  addInstruction $ IR.Jmp endLabel
  startBlock falseLabel currentLabel
  falseVal <- compileExpression falseExpr
  addInstruction $ IR.Jmp endLabel
  startBlock endLabel currentLabel
  outReg <- getNextRegister
  addInstruction $ IR.Phi outReg [(trueLabel, trueVal), (falseLabel, falseVal)]
  return $ IR.Register outReg
compileExpression (AST.AssignExpr var expr) = compileAssignment var expr


irBinOp :: String -> IR.BinOpType
irBinOp "+" = IR.Add
irBinOp "-" = IR.Sub
irBinOp "*" = IR.Mul
irBinOp "/" = IR.Div
irBinOp "==" = IR.Eq
irBinOp "<" = IR.Lt
irBinOp ">" = IR.Gt
irBinOp "<=" = IR.Le
irBinOp ">=" = IR.Ge
irBinOp "!=" = IR.Ne
irBinOp _ = error "Unknown binary operator"