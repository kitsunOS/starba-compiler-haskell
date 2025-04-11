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
  VarDeclarationValue VariableDefinition |
  FuncDeclarationValue FunctionDefinition |
  EnumDeclarationValue EnumDefinition
  deriving (Show, Eq)

newtype InnerDeclarationValue =
  InnerVarDeclarationValue VariableDefinition
  deriving (Show, Eq)

data VariableDefinition = VariableDefinition {
  variableType :: Type,
  initializer :: Maybe Expression
} deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition {
  functionDefParameters :: [VariableDefinition],
  functionDefType :: Type,
  functionDefBody :: Maybe FunctionBody
} deriving (Show, Eq)

newtype FunctionBody = FunctionBody {
  functionBodyStatements :: [Statement]
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
  InnerDecl InnerDeclaration
  | Assignment String Expression
  | Return (Maybe Expression)
  | If Expression Statement (Maybe Statement)
  | While Expression Statement
  | For (Maybe AST.InnerDeclaration) (Maybe Expression) (Maybe Expression) Statement
  | BlockBody [Statement]
  deriving (Show, Eq)

data Expression =
  NumberLiteral Integer
  | StringLiteral String
  | Variable String
  | BinOp String Expression Expression
  | Ternary Expression Expression Expression
  | AssignExpr String Expression
  deriving (Show, Eq)

data Type = Type {
  typeName :: String
} | Void deriving (Show, Eq)