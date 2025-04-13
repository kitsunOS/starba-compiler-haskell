module AST.AST where

newtype Module sym = Module {
  declarations :: [Declaration sym]
} deriving (Show, Eq)

data Declaration sym = Declaration {
  declName :: sym,
  declVisibility :: Visibility,
  declValue :: DeclarationValue sym
} deriving (Show, Eq)

data InnerDeclaration sym = InnerDeclaration {
  iDeclName :: sym,
  iDeclValue :: InnerDeclarationValue sym
} deriving (Show, Eq)

data Parameter sym = Parameter {
  paramName :: sym,
  paramType :: Type sym
} deriving (Show, Eq)

data Visibility =
  Public |
  Private
  deriving (Show, Eq)

data DeclarationValue sym =
  VarDeclarationValue (VariableDefinition sym) |
  FuncDeclarationValue (FunctionDefinition sym) |
  EnumDeclarationValue (EnumDefinition sym)
  deriving (Show, Eq)

newtype InnerDeclarationValue sym =
  InnerVarDeclarationValue (VariableDefinition sym)
  deriving (Show, Eq)

data VariableDefinition sym = VariableDefinition {
  variableType :: Type sym,
  initializer :: Maybe (Expression sym)
} deriving (Show, Eq)

data FunctionDefinition sym = FunctionDefinition {
  functionDefParameters :: [Parameter sym],
  functionDefType :: Type sym,
  functionDefBody :: Maybe (FunctionBody sym)
} deriving (Show, Eq)

newtype FunctionBody sym = FunctionBody {
  functionBodyStatements :: [Statement sym]
} deriving (Show, Eq)

data EnumDefinition sym = EnumDefinition {
  enumName :: sym,
  enumValues :: [EnumValue sym],
  enumMembers :: [InnerDeclaration sym]
} deriving (Show, Eq)

data EnumValue sym = EnumValue {
  enumValueName :: sym,
  enumParameters :: [Parameter sym],
  enumMemberAssign :: [EnumMemberAssign sym]
} deriving (Show, Eq)

data EnumMemberAssign sym = EnumMember {
  enumMemberName :: sym,
  enumMemberValue :: Expression sym
} deriving (Show, Eq)

data Statement sym =
  InnerDecl (InnerDeclaration sym)
  | Assignment sym (Expression sym)
  | Return (Maybe (Expression sym))
  | If (Expression sym) (Statement sym) (Maybe (Statement sym))
  | While (Expression sym) (Statement sym)
  | For (Maybe (InnerDeclaration sym)) (Maybe (Expression sym)) (Maybe (Expression sym)) (Statement sym)
  | BlockBody [Statement sym]
  deriving (Show, Eq)

data Expression sym =
  NumberLiteral Integer
  | StringLiteral String
  | Variable sym
  | BinOp String (Expression sym) (Expression sym)
  | Ternary (Expression sym) (Expression sym) (Expression sym)
  | AssignExpr sym (Expression sym)
  deriving (Show, Eq)

data Type sym = Type {
  typeName :: sym
} | Void deriving (Show, Eq)

data Symbol = Symbol
  { symbolName :: String
  , symbolId :: Int
  } deriving (Eq, Ord)

instance Show Symbol where
  show (Symbol name id) = name ++ "#" ++ show id