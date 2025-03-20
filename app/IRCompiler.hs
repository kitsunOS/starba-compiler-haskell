module IRCompiler (compileModule) where

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
        (procedures, symbolTable') = compileFuncBody body symbolTable
      in
        DeclarationContribution procedures symbolTable'
  
compileFuncBody :: AST.FunctionBody -> SymbolTable -> ([Procedure], SymbolTable)
compileFuncBody (AST.FunctionBody stmts) symbolTable =
  let
    function = foldl compileStatement (PartialFunction [] symbolTable (FuncBodyContext Map.empty (RegName "a" 0))) stmts
  in
    case function of
      PartialFunction procedures symbolTable _ -> (procedures, symbolTable)
    
  
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
compileInnerVarDeclaration partialFunction name (AST.VariableDefinition typ initializer) =
  let
    (PartialFunction procedures symbolTable context) = partialFunction
    (regName, context') = allocateRegister name context
  in
    PartialFunction procedures symbolTable context'

compileEnumDeclaration :: String -> AST.Visibility -> AST.EnumDefinition -> DeclarationContribution
compileEnumDeclaration name visibility (AST.EnumDefinition _ values members) =
  -- TODO: Implement
  let
    symbolTable = SymbolTable $ Map.singleton name (IntLiteral 0)
  in
    DeclarationContribution [] symbolTable

mergeSymbolTables :: [SymbolTable] -> SymbolTable
mergeSymbolTables = foldl mergeSymbolTable (SymbolTable Map.empty)

mergeSymbolTable :: SymbolTable -> SymbolTable -> SymbolTable
mergeSymbolTable (SymbolTable table1) (SymbolTable table2) =
  SymbolTable $ Map.union table1 table2

allocateRegister :: String -> FuncBodyContext -> (RegName, FuncBodyContext)
allocateRegister name (FuncBodyContext activeRegisters nextRegister) =
  let
    regName = nextRegister
    nextRegister' = nextRegName nextRegister
    activeRegisters' = Map.insert name regName activeRegisters
  in
    (regName, FuncBodyContext activeRegisters' nextRegister')

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
  activeRegisters :: Map.Map String RegName,
  nextRegister :: RegName
} deriving (Show)

data PartialFunction = PartialFunction [Procedure] SymbolTable FuncBodyContext 
  deriving (Show)