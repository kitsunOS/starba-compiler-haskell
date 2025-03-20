module IR where
import Data.Map (Map)

data Module = Module [Procedure] SymbolTable
  deriving (Show, Eq)

newtype Procedure = Procedure [Instruction]
  deriving (Show, Eq)

data RegName = RegName String Int
  deriving (Show, Eq)

data Value
  = Register RegName
  | Immediate Int
  | LabelReference LabelRef
  | SymbolReference Symbol
  deriving (Show, Eq)

newtype LabelRef = LabelRef String
  deriving (Show, Eq)

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
  deriving (Show, Eq)

newtype SymbolTable = SymbolTable {
  table :: Map String Literal
} deriving (Show, Eq)