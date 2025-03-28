module IR where
import Data.Map (Map)

data Module = Module [Procedure] FieldTable SymbolTable
  deriving (Show, Eq)

newtype Procedure = Procedure [Block]
  deriving (Show, Eq)

data Block = Block {
  blockLabel :: LabelRef,
  blockInstructions :: [Instruction]
} deriving (Eq)

instance Show Block where
  show (Block label instructions) = show label ++ ":\n" ++ unlines (map (\i -> "  " ++ show i) instructions)

data RegName = RegName String Int
  deriving (Eq, Ord)

instance Show RegName where
  show (RegName name num) = "%" ++ name ++ "." ++ show num

data Value
  = Register RegName
  | Immediate Integer
  | LabelReference LabelRef
  | SymbolReference String
  deriving (Eq)

instance Show Value where
  show (Register reg) = show reg
  show (Immediate i) = show i
  show (LabelReference (LabelRef l)) = l
  show (SymbolReference s) = s

type VarRef = String

newtype LabelRef = LabelRef String
  deriving (Eq, Ord)

instance Show LabelRef where
  show (LabelRef l) = "[" ++ l ++ "]"

newtype Symbol = Symbol String
  deriving (Show, Eq)

data Instruction
  = Ret (Maybe Value)
  | Set Value Value
  | BinOp BinOpType Value Value Value
  deriving (Eq)

instance Show Instruction where
  show (Ret v) = "ret " ++ show v
  show (Set v1 v2) = "set " ++ show v1 ++ " " ++ show v2
  show (BinOp op v1 v2 v3) = show op ++ " " ++ show v1 ++ " " ++ show v2 ++ " " ++ show v3

data BinOpType
  = Add | Sub | Mul | Div
  deriving (Eq)

instance Show BinOpType where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

data Literal
  = IntLiteral Int
  | StringLiteral String
  deriving (Eq, Ord)

instance Show Literal where
  show (IntLiteral i) = show i
  show (StringLiteral s) = s

data SymbolTable = SymbolTable {
  symbolMap :: Map String Literal,
  reverseMap :: Map Literal String
} deriving (Show, Eq)

newtype FieldTable = FieldTable {
  fieldMap :: Map String Literal
} deriving (Show, Eq)