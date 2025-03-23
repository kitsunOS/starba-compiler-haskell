module IRGen (compileModule) where

import AST
import IR
import qualified Data.Map as Map

compileModule :: AST.Module -> IR.Module
compileModule (AST.Module decls) =
  let
    declContributions = map compileDeclaration decls
    procedures = concatMap declProcedures declContributions
    symbolTable = mergeSymbolTables $ map declSymbolTable declContributions
    fieldTable = mergeFieldTables $ map declFieldTable declContributions
  in
    IR.Module procedures fieldTable symbolTable

compileDeclaration :: AST.Declaration -> DeclarationContribution
compileDeclaration (AST.Declaration name visibility value) =
  case value of
    AST.VarDeclarationValue varDef ->
      compileVarDeclaration name visibility varDef
    AST.FuncDeclarationValue funcDef ->
      compileFuncDeclaration name visibility funcDef
    AST.EnumDeclarationValue enumDef ->
      compileEnumDeclaration name visibility enumDef

compileVarDeclaration :: String -> AST.Visibility -> AST.VariableDefinition -> DeclarationContribution
compileVarDeclaration name visibility (AST.VariableDefinition typ initializer) =
  let
    -- TODO: Evaluate the actual initializer
    fieldTable = FieldTable $ Map.singleton name (IntLiteral 0)
  in
    DeclarationContribution [] (SymbolTable Map.empty Map.empty) fieldTable
  
compileFuncDeclaration :: String -> AST.Visibility -> AST.FunctionDefinition -> DeclarationContribution
compileFuncDeclaration name visibility (AST.FunctionDefinition params typ bodyMaybe) =
  case bodyMaybe of
    Nothing -> error "Function declaration without body not yet supported"
    Just body ->
      let
        symbolTable = SymbolTable Map.empty Map.empty
        (instructions, symbolTable') = compileFuncBody body symbolTable
      in
        DeclarationContribution [Procedure instructions] symbolTable' (FieldTable Map.empty)
  
compileFuncBody :: AST.FunctionBody -> SymbolTable -> ([Instruction], SymbolTable)
compileFuncBody (AST.FunctionBody stmts) symbolTable =
  let
    funcBodyContext = FuncBodyContext Map.empty (RegName "a" 0) (LabelRef "entry") "a"
    function = foldl compileStatement (PartialFunction [] symbolTable funcBodyContext) stmts
  in
    case function of
      PartialFunction instructions symbolTable _ -> (instructions, symbolTable)
    
  
compileStatement :: PartialFunction -> AST.Statement -> PartialFunction
compileStatement partialFunction stmt =
  case stmt of
    AST.InnerDecl innerDecl ->
      compileInnerDeclaration partialFunction innerDecl

compileInnerDeclaration :: PartialFunction -> AST.InnerDeclaration -> PartialFunction
compileInnerDeclaration partialFunction (AST.InnerDeclaration name value) =
  case value of
    AST.InnerVarDeclarationValue varDef ->
      compileInnerVarDeclaration partialFunction name varDef

compileInnerVarDeclaration :: PartialFunction -> String -> AST.VariableDefinition -> PartialFunction
compileInnerVarDeclaration partialFunction name (AST.VariableDefinition typ Nothing) = error "Inner variable declaration without initializer not yet supported"
compileInnerVarDeclaration partialFunction name (AST.VariableDefinition typ (Just initializer)) =
  let
    (regName, partialFunction') = compileExpression partialFunction initializer
    context = pfContext partialFunction'
    varRef = VarRef name 0
    newActiveRegisters = Map.alter updateInnerMap varRef (activeRegisters context)
      where
        updateInnerMap Nothing = Just $ Map.singleton (activeLabel context) regName
        updateInnerMap (Just labelMap) = Just $ Map.insert (activeLabel context) regName labelMap
    context' = context { activeRegisters = newActiveRegisters }
  in
    partialFunction' { pfContext = context' }

compileEnumDeclaration :: String -> AST.Visibility -> AST.EnumDefinition -> DeclarationContribution
compileEnumDeclaration name visibility (AST.EnumDefinition _ values members) =
  -- TODO: Implement
  DeclarationContribution [] (SymbolTable Map.empty Map.empty) (FieldTable Map.empty)

compileExpression :: PartialFunction -> AST.Expression -> (RegName, PartialFunction)
compileExpression partialFunction (NumberLiteral num) =
  let
    (PartialFunction instructions symbolTable context) = partialFunction
    (regName, context') = allocateRegister context
    instructions' = instructions ++ [Set (Register regName) (Immediate num)]
  in
    (regName, PartialFunction instructions' symbolTable context')
compileExpression partialFunction (AST.StringLiteral str) =
  let
    (symbolName, partialFunction') = allocateSymbol partialFunction str
    (regName, context') = allocateRegister (pfContext partialFunction')
    instructions' = pfInstructions partialFunction' ++ [Set (Register regName) (SymbolReference symbolName)]
  in
    (regName, partialFunction' { pfInstructions = instructions' })

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

allocateRegister :: FuncBodyContext -> (RegName, FuncBodyContext)
allocateRegister funcBodyContext =
  let
    regName = nextRegister funcBodyContext
    nextRegister' = nextRegName (nextRegister funcBodyContext)
  in
    (regName, funcBodyContext { nextRegister = nextRegister' })

allocateSymbol :: PartialFunction -> String -> (String, PartialFunction)
allocateSymbol partialFunction value =
  let
    context = pfContext partialFunction
    currentSymbolTable = pfSymbolTable partialFunction
    lit = IR.StringLiteral value
    existingSymbol = Map.lookup lit (reverseMap currentSymbolTable)
  in case existingSymbol of
    Just name -> (name, partialFunction)  -- Reuse existing symbol
    Nothing ->
      let
        symbolName = nextSymbolName context
        symbolMap' = Map.insert symbolName lit (symbolMap currentSymbolTable)
        reverseMap' = Map.insert lit symbolName (reverseMap currentSymbolTable)
        symbolTable' = SymbolTable symbolMap' reverseMap'
        context' = context { nextSymbolName = incrementRegName symbolName }
      in
        (symbolName, partialFunction {
          pfSymbolTable = symbolTable',
          pfContext = context'
        })


nextRegName :: RegName -> RegName
nextRegName (RegName prefix num) =
  RegName (incrementRegName prefix) 0

incrementRegName :: String -> String
incrementRegName = reverse . increment . reverse
  where
    increment [] = ['a']
    increment ('z':xs) = 'a' : increment xs
    increment (x:xs) = succ x : xs

data DeclarationContribution = DeclarationContribution {
  declProcedures :: [Procedure],
  declSymbolTable :: SymbolTable,
  declFieldTable :: FieldTable
}

data FuncBodyContext = FuncBodyContext {
  activeRegisters :: Map.Map VarRef (Map.Map LabelRef RegName),
  nextRegister :: RegName,
  activeLabel :: LabelRef,
  nextSymbolName :: String
} deriving (Show)

data PartialFunction = PartialFunction {
  pfInstructions :: [Instruction],
  pfSymbolTable :: SymbolTable,
  pfContext :: FuncBodyContext
} deriving (Show)