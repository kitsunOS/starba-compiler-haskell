module Frontend.Starba.AST.ASTSymbolRes where

import qualified Data.Map as Map

import qualified Frontend.Starba.AST.AST as AST

type SSym = String
type DSym = AST.Symbol

-- Scope helper

data ActiveScope = ActiveScope {
  activeScopeParent :: Maybe ActiveScope,
  activeScopeSymbolMap :: Map.Map SSym DSym
}

newSymbol :: ActiveScope -> SSym -> (ActiveScope, DSym)
newSymbol scope name =
  let newId = case Map.lookup name (activeScopeSymbolMap scope) of
        Just symbol -> error $ "Duplicate symbol: " ++ name
        Nothing -> case lookupSymbol scope name of
          Just (AST.Symbol name' prevId) -> AST.Symbol name' (prevId + 1)
          Nothing -> AST.Symbol name 0
  in (ActiveScope (Just scope) (Map.insert name newId (activeScopeSymbolMap scope)), newId)

lookupSymbol :: ActiveScope -> SSym -> Maybe DSym
lookupSymbol scope name = case Map.lookup name (activeScopeSymbolMap scope) of
  Just symbol -> Just symbol
  Nothing -> case activeScopeParent scope of
    Just parent -> lookupSymbol parent name
    Nothing -> Nothing

requireSymbol :: ActiveScope -> SSym -> DSym
requireSymbol scope name =
  case lookupSymbol scope name of
    Just symbol -> symbol
    Nothing -> error $ "Symbol not found: " ++ name

newScope :: ActiveScope -> ActiveScope
newScope parent =
  ActiveScope (Just parent) Map.empty

parseManyScoped :: (ActiveScope -> a -> (ActiveScope, b)) -> [a] -> ActiveScope -> (ActiveScope, [b])
parseManyScoped f xs scope =
  foldl (\(scope', acc) x ->
    let (scope'', newX) = f scope' x
    in (scope'', acc ++ [newX])) (scope, []) xs

-- Functions

resolveSymbols :: AST.Module SSym -> AST.Module DSym
resolveSymbols = resolveModule (ActiveScope Nothing Map.empty)

resolveModule :: ActiveScope -> AST.Module SSym -> AST.Module DSym
resolveModule scope (AST.Module declarations) =
  let initialScope = foldl resolveDeclarationShallow scope declarations
      resolvedDeclarations = map (resolveDeclarationDeep initialScope) declarations
  in AST.Module resolvedDeclarations


resolveDeclarationShallow :: ActiveScope -> AST.Declaration SSym -> ActiveScope
resolveDeclarationShallow scope (AST.Declaration name visibility value) = let (scope', newName) = newSymbol scope name in scope'

resolveDeclarationDeep :: ActiveScope -> AST.Declaration SSym -> AST.Declaration DSym
resolveDeclarationDeep scope (AST.Declaration name visibility value) =
  let newName = requireSymbol scope name
      newValue = case value of
        AST.VarDeclarationValue varDef -> AST.VarDeclarationValue (resolveVariableDefinition scope varDef)
        AST.FuncDeclarationValue funcDef -> AST.FuncDeclarationValue (resolveFunctionDefinition scope funcDef)
        AST.EnumDeclarationValue enumDef -> AST.EnumDeclarationValue (resolveEnumDefinition scope enumDef)
  in AST.Declaration newName visibility newValue

resolveVariableDefinition :: ActiveScope -> AST.VariableDefinition SSym -> AST.VariableDefinition DSym
resolveVariableDefinition scope (AST.VariableDefinition typ Nothing) = AST.VariableDefinition (resolveType scope typ) Nothing
resolveVariableDefinition scope (AST.VariableDefinition typ (Just initializer)) =
  let newType = resolveType scope typ
      newInitializer = resolveExpression scope initializer
  in AST.VariableDefinition newType (Just newInitializer)

resolveFunctionDefinition :: ActiveScope -> AST.FunctionDefinition SSym -> AST.FunctionDefinition DSym
resolveFunctionDefinition scope (AST.FunctionDefinition parameters typ body) =
  let (innerScope, newParameters) = parseManyScoped resolveParameter parameters scope
      newTyp = resolveType scope typ
      newBody = case body of
        Just (AST.FunctionBody statements) -> Just (AST.FunctionBody statements')
          where (_, statements') = parseManyScoped resolveStatement statements (newScope innerScope)
        Nothing -> Nothing
  in AST.FunctionDefinition newParameters newTyp newBody

resolveEnumDefinition :: ActiveScope -> AST.EnumDefinition SSym -> AST.EnumDefinition DSym
resolveEnumDefinition scope (AST.EnumDefinition name values members) =
  let newName = requireSymbol scope name
      newValues = map (resolveEnumValue scope) values
      (_, newMembers) = parseManyScoped resolveInnerDeclaration members (newScope scope)
  in AST.EnumDefinition newName newValues newMembers

resolveEnumValue :: ActiveScope -> AST.EnumValue SSym -> AST.EnumValue DSym
resolveEnumValue scope (AST.EnumValue name parameters memberAssign) =
  let newName = requireSymbol scope name
      (scope', newParameters) = parseManyScoped resolveParameter parameters scope
      newMemberAssign = map (resolveEnumMemberAssign scope) memberAssign
  in AST.EnumValue newName newParameters newMemberAssign

resolveEnumMemberAssign :: ActiveScope -> AST.EnumMemberAssign SSym -> AST.EnumMemberAssign DSym
resolveEnumMemberAssign scope (AST.EnumMember name value) = AST.EnumMember (requireSymbol scope name) (resolveExpression scope value)

resolveParameter :: ActiveScope -> AST.Parameter SSym -> (ActiveScope, AST.Parameter DSym)
resolveParameter scope (AST.Parameter name typ) = 
  let (scope', newName) = newSymbol scope name
      newType = resolveType scope' typ
  in (scope', AST.Parameter newName newType)

resolveStatement :: ActiveScope -> AST.Statement SSym -> (ActiveScope, AST.Statement DSym)
resolveStatement scope (AST.InnerDecl innerDecl) =
  let (scope', newDecl) = resolveInnerDeclaration scope innerDecl
  in (scope', AST.InnerDecl newDecl)
resolveStatement scope (AST.Assignment name expr) = (scope, AST.Assignment (requireSymbol scope name) (resolveExpression scope expr))
resolveStatement scope (AST.Return (Just expr)) = (scope, AST.Return (Just (resolveExpression scope expr)))
resolveStatement scope (AST.Return Nothing) = (scope, AST.Return Nothing)
resolveStatement scope (AST.If cond true false) =
  let newCond = resolveExpression scope cond
      (_, newTrue) = resolveStatement (newScope scope) true
      (_, newFalse) = case false of
        Just falseStmt -> let (_, newFalseStmt) = resolveStatement (newScope scope) falseStmt in (scope, Just newFalseStmt)
        Nothing -> (scope, Nothing)
  in (scope, AST.If newCond newTrue newFalse)
resolveStatement scope (AST.While cond body) =
  let newCond = resolveExpression scope cond
      (_, newBody) = resolveStatement (newScope scope) body
  in (scope, AST.While newCond newBody)
resolveStatement scope (AST.For init cond iter body) =
  let (scope', newInit) = case init of
        Just innerDecl -> let (scope'', newDecl) = resolveInnerDeclaration scope innerDecl in (scope'', Just newDecl)
        Nothing -> (scope, Nothing)
      newCond = case cond of
        Just expr -> Just (resolveExpression scope' expr)
        Nothing -> Nothing
      newIter = case iter of
        Just expr -> Just (resolveExpression scope' expr)
        Nothing -> Nothing
      (_, newBody) = resolveStatement (newScope scope') body
  in (scope, AST.For newInit newCond newIter newBody)
resolveStatement scope (AST.BlockBody statements) =
  let (_, newStatements) = parseManyScoped resolveStatement statements (newScope scope)
  in (scope, AST.BlockBody newStatements)
  

resolveInnerDeclaration :: ActiveScope -> AST.InnerDeclaration SSym -> (ActiveScope, AST.InnerDeclaration DSym)
resolveInnerDeclaration scope (AST.InnerDeclaration name (AST.InnerVarDeclarationValue varDef)) =
  let newVarDef = resolveVariableDefinition scope varDef
      (scope', newName) = newSymbol scope name
      newDecl = AST.InnerDeclaration newName (AST.InnerVarDeclarationValue newVarDef)
  in (scope', newDecl)

resolveExpression :: ActiveScope -> AST.Expression SSym -> AST.Expression DSym
resolveExpression scope (AST.NumberLiteral num) = AST.NumberLiteral num
resolveExpression scope (AST.StringLiteral str) = AST.StringLiteral str
resolveExpression scope (AST.Variable name) = AST.Variable (requireSymbol scope name)
resolveExpression scope (AST.BinOp op left right) = AST.BinOp op (resolveExpression scope left) (resolveExpression scope right)
resolveExpression scope (AST.Ternary cond true false) =
  AST.Ternary (resolveExpression scope cond) (resolveExpression scope true) (resolveExpression scope false)
resolveExpression scope (AST.AssignExpr name value) = AST.AssignExpr (requireSymbol scope name) (resolveExpression scope value)

resolveType :: ActiveScope -> AST.Type SSym -> AST.Type DSym
resolveType scope (AST.Type name) = AST.Type (AST.Symbol name 0) -- TODO: Actually resolve the type
resolveType _ AST.Void = AST.Void