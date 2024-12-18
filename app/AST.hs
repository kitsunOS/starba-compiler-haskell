module AST where

newtype Module = Module {
  declarations :: [Declaration]
} deriving (Show, Eq)

data Declaration = Declaration {
  declName :: String,
  declVisibility :: Visibility,
  declValue :: DeclarationValue
} deriving (Show, Eq)

data InnerDeclaration = InnerDeclaration {
  iDeclName :: String,
  iDeclValue :: InnerDeclarationValue
} deriving (Show, Eq)

data Visibility =
  Public |
  Private
  deriving (Show, Eq)

data DeclarationValue =
  VarDef VariableDefinition |
  VarDecl VariableDefinition Expression |
  FuncDef FunctionDefinition |
  FuncDecl FunctionDefinition FunctionBody |
  EnumDecl EnumDefinition
  deriving (Show, Eq)

data InnerDeclarationValue =
  InnerVarDef VariableDefinition |
  InnerVarDecl VariableDefinition Expression
  deriving (Show, Eq)

newtype VariableDefinition = VariableDefinition {
  variableType :: Type
} deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition {
  functionParameters :: [VariableDefinition],
  functionType :: Type
} deriving (Show, Eq)

newtype FunctionBody = FunctionBody {
  functionBody :: [Statement]
} deriving (Show, Eq)

data EnumDefinition = EnumDefinition {
  enumName :: String,
  enumValues :: [EnumValue],
  enumMembers :: [InnerDeclaration]
} deriving (Show, Eq)

data EnumValue = EnumValue {
  enumValueName :: String,
  enumParameters :: [VariableDefinition],
  enumMemberAssign :: [EnumMemberAssign]
} deriving (Show, Eq)

data EnumMemberAssign = EnumMember {
  enumMemberName :: String,
  enumMemberValue :: Expression
} deriving (Show, Eq)

data Statement =
  InnerDecl InnerDeclaration |
  Assignment VariableDefinition Expression
  deriving (Show, Eq)

data Expression =
  NumberLiteral Integer |
  StringLiteral String
  deriving (Show, Eq)

data Type = Type {
  typeName :: String
} | Void deriving (Show, Eq)