module IR where
import Data.Map (Map)

data Module = Module [Procedure] FieldTable SymbolTable
  deriving (Show, Eq)

newtype Procedure = Procedure [Instruction]
  deriving (Show, Eq)

data RegName = RegName String Int
  deriving (Show, Eq)

data Value
  = Register RegName
  | Immediate Integer
  | LabelReference LabelRef
  | SymbolReference String
  deriving (Show, Eq)

data VarRef = VarRef String Integer
  deriving (Show, Eq, Ord)

newtype LabelRef = LabelRef String
  deriving (Show, Eq, Ord)

newtype Symbol = Symbol String
  deriving (Show, Eq)

data Instruction
  = Label LabelRef
  | Ret Value
  | Set Value Value
  deriving (Show, Eq)

data Literal
  = IntLiteral Int
  | StringLiteral String
  deriving (Show, Eq, Ord)

data SymbolTable = SymbolTable {
  symbolMap :: Map String Literal,
  reverseMap :: Map Literal String
} deriving (Show, Eq)

newtype FieldTable = FieldTable {
  fieldMap :: Map String Literal
} deriving (Show, Eq)