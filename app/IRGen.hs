module IRGen (compileModule) where

import qualified AST
import IR
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Set as Set

-- Data types
data ActiveScope = ActiveScope {
  activeScopeParent :: Maybe ActiveScope,
  activeScopeRegisters :: Map.Map VarRef (Map.Map LabelRef RegName)
}

data IRGenD = IRGen {
  irgBlocks :: [Block],
  irgSymbolTable :: SymbolTable,
  irgNextRegister :: RegName,
  irgNextSymbolName :: String,
  irgActiveScope :: ActiveScope,
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
  irgActiveScope = ActiveScope Nothing Map.empty,
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
  modify $ \s -> s { irgActiveScope = inheritScope label (irgActiveScope s) inheritedLabel }
  return ()
  where
    inheritScope :: LabelRef -> ActiveScope -> LabelRef -> ActiveScope
    inheritScope newLabel (ActiveScope parent activeRegisters) inheritedLabel = case parent of
      Just parentScope -> ActiveScope (Just (inheritScope newLabel parentScope inheritedLabel)) (inheritVars newLabel activeRegisters inheritedLabel)
      Nothing -> ActiveScope Nothing (inheritVars newLabel activeRegisters inheritedLabel)
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

-- Variable helpers
createVar :: String -> RegName -> IRGenM ()
createVar name regName = do
  activeScope <- gets irgActiveScope
  activeLabel <- gets irgActiveLabel
  case Map.lookup name (activeScopeRegisters activeScope) of
    Just regMap -> case Map.lookup activeLabel regMap of
      Just _ -> throwError $ "Variable " ++ name ++ " already defined in this scope"
      Nothing -> do
        let newRegMap = Map.insert activeLabel regName regMap
            newActiveScope = ActiveScope (Just activeScope) (Map.insert name newRegMap (activeScopeRegisters activeScope))
        modify $ \s -> s { irgActiveScope = newActiveScope }
        return ()
    Nothing -> do
      let newRegMap = Map.singleton activeLabel regName
          newActiveScope = ActiveScope (Just activeScope) (Map.insert name newRegMap (activeScopeRegisters activeScope))
      modify $ \s -> s { irgActiveScope = newActiveScope }
      return ()

lookupVar :: String -> IRGenM (Maybe RegName)
lookupVar name = do
  activeScope <- gets irgActiveScope
  let regName = lookupVar' name activeScope
  case regName of
    Just reg -> return (Just reg)
    Nothing -> throwError $ "Variable " ++ name ++ " not found in scope"
  where
    lookupVar' :: String -> ActiveScope -> Maybe RegName
    lookupVar' name (ActiveScope parent activeRegisters) = case Map.lookup name activeRegisters of
      Just regMap -> case Map.lookup (irgActiveLabel defaultState) regMap of
        Just regName -> Just regName
        Nothing -> case parent of
          Just parentScope -> lookupVar' name parentScope
          Nothing -> Nothing
      Nothing -> case parent of
        Just parentScope -> lookupVar' name parentScope
        Nothing -> Nothing

updateVar :: String -> RegName -> IRGenM ()
updateVar name regName = do
  activeScope <- gets irgActiveScope
  activeLabel <- gets irgActiveLabel
  case updateVar' name regName activeScope of
    Right newActiveScope -> modify $ \s -> s { irgActiveScope = newActiveScope }
    Left err -> throwError err
  where
    updateVar' :: String -> RegName -> ActiveScope -> Either String ActiveScope
    updateVar' name regName (ActiveScope parent activeRegisters) = case Map.lookup name activeRegisters of
      Just regMap -> let newRegMap = Map.insert (irgActiveLabel defaultState) regName regMap
                     in Right $ ActiveScope parent (Map.insert name newRegMap activeRegisters)
      Nothing -> case parent of
        Just parentScope -> case updateVar' name regName parentScope of
            Right newParentScope -> Right $ ActiveScope (Just newParentScope) activeRegisters
            Left err -> Left err
        Nothing -> Left $ "Variable " ++ name ++ " not found in scope"

startActiveScope :: IRGenM ()
startActiveScope = do
  activeScope <- gets irgActiveScope
  let newActiveScope = ActiveScope (Just activeScope) Map.empty
  modify $ \s -> s { irgActiveScope = newActiveScope }
  return ()

endActiveScope :: IRGenM ()
endActiveScope = do
  activeScope <- gets irgActiveScope
  case activeScopeParent activeScope of
    Just parent -> modify $ \s -> s { irgActiveScope = parent }
    Nothing -> throwError "No parent scope to return to"
  return ()

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
compileStatement (AST.Assignment varName expr) = void $ compileAssignment varName expr
compileStatement (AST.BlockBody stmts) = do
  startActiveScope
  mapM_ compileStatement stmts
  endActiveScope

compileInnerDeclaration :: AST.InnerDeclaration -> IRGenM ()
compileInnerDeclaration (AST.InnerDeclaration name (AST.InnerVarDeclarationValue varDef)) = compileInnerVarDeclaration name varDef

compileInnerVarDeclaration :: String -> AST.VariableDefinition -> IRGenM ()
compileInnerVarDeclaration name (AST.VariableDefinition typ Nothing) = throwError "Inner variable declaration without initializer not yet supported"
compileInnerVarDeclaration name (AST.VariableDefinition typ (Just initializer)) = do
  regName <- compileExpression initializer
  createVar name regName

compileReturn :: Maybe AST.Expression -> IRGenM ()
compileReturn Nothing = addInstruction $ Ret Nothing
compileReturn (Just expr) = do
  regName <- compileExpression expr
  addInstruction $ Ret (Just (Register regName))

compileAssignment :: String -> AST.Expression -> IRGenM RegName
compileAssignment varName expr = do
  varValue <- lookupVar varName
  case varValue of
    Just reg -> do
      regName <- compileExpression expr
      updateVar varName regName
      return regName
    Nothing -> throwError $ "Variable " ++ varName ++ " not in scope"

compileExpression :: AST.Expression -> IRGenM RegName
compileExpression (AST.NumberLiteral num) = do
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
compileExpression (AST.Variable varName) = do
  varValue <- lookupVar varName
  case varValue of
    Just regName -> return regName
    Nothing -> throwError $ "Variable " ++ varName ++ " not in scope"
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
compileExpression (AST.AssignExpr var expr) = compileAssignment var expr


irBinOp :: String -> BinOpType
irBinOp "+" = Add
irBinOp "-" = Sub
irBinOp "*" = Mul
irBinOp "/" = Div
irBinOp "==" = Eq
irBinOp _ = error "Unknown binary operator"