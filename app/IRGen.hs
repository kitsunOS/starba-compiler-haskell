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
  in
    IR.Module procedures symbolTable

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
    symbolTable = SymbolTable $ Map.singleton name (IntLiteral 0)
  in
    DeclarationContribution [] symbolTable
  
compileFuncDeclaration :: String -> AST.Visibility -> AST.FunctionDefinition -> DeclarationContribution
compileFuncDeclaration name visibility (AST.FunctionDefinition params typ bodyMaybe) =
  case bodyMaybe of
    Nothing -> error "Function declaration without body not yet supported"
    Just body ->
      let
        symbolTable = SymbolTable Map.empty
        (instructions, symbolTable') = compileFuncBody body symbolTable
      in
        DeclarationContribution [Procedure instructions] symbolTable'
  
compileFuncBody :: AST.FunctionBody -> SymbolTable -> ([Instruction], SymbolTable)
compileFuncBody (AST.FunctionBody stmts) symbolTable =
  let
    function = foldl compileStatement (PartialFunction [] symbolTable (FuncBodyContext Map.empty (RegName "a" 0) (LabelRef "entry"))) stmts
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
    (regName, PartialFunction instructions symbolTable context) = compileExpression partialFunction initializer
    varRef = VarRef name 0
    newActiveRegisters = Map.alter updateInnerMap varRef (activeRegisters context)
      where
        updateInnerMap Nothing = Just $ Map.singleton (activeLabel context) regName
        updateInnerMap (Just labelMap) = Just $ Map.insert (activeLabel context) regName labelMap
    context' = FuncBodyContext newActiveRegisters (nextRegister context) (activeLabel context)
  in
    PartialFunction instructions symbolTable context'

compileEnumDeclaration :: String -> AST.Visibility -> AST.EnumDefinition -> DeclarationContribution
compileEnumDeclaration name visibility (AST.EnumDefinition _ values members) =
  -- TODO: Implement
  DeclarationContribution [] (SymbolTable Map.empty)

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
    (PartialFunction instructions symbolTable context) = partialFunction
    (regName, context') = allocateRegister context
    instructions' = instructions ++ [Set (Register regName) (SymbolReference (Symbol str))]
  in
    (regName, PartialFunction instructions' symbolTable context')

mergeSymbolTables :: [SymbolTable] -> SymbolTable
mergeSymbolTables = foldl mergeSymbolTable (SymbolTable Map.empty)

mergeSymbolTable :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTable (SymbolTable table1) (SymbolTable table2) =
  SymbolTable $ Map.union table1 table2

allocateRegister :: FuncBodyContext -> (RegName, FuncBodyContext)
allocateRegister funcBodyContext =
  let
    regName = nextRegister funcBodyContext
    nextRegister' = nextRegName (nextRegister funcBodyContext)
  in
    (regName, FuncBodyContext (activeRegisters funcBodyContext) nextRegister' (activeLabel funcBodyContext))

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
  declSymbolTable :: SymbolTable
}

data FuncBodyContext = FuncBodyContext {
  activeRegisters :: Map.Map VarRef (Map.Map LabelRef RegName),
  nextRegister :: RegName,
  activeLabel :: LabelRef
} deriving (Show)

data PartialFunction = PartialFunction [Instruction] SymbolTable FuncBodyContext 
  deriving (Show)