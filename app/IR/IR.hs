module IR.IR where

import qualified AST.AST as AST
import Data.Map (Map)
import Data.List (intercalate)

data Module = Module [Procedure] FieldTable SymbolTable
  deriving (Eq)

instance Show Module where
  show (Module procedures fieldTable symbolTable) = intercalate "\n" (map show procedures) ++
    "\n" ++ show fieldTable ++ "\n" ++ show symbolTable

newtype Procedure = Procedure [Block]
  deriving (Eq)

instance Show Procedure where
  show (Procedure blocks) = unlines $ map show blocks

data Block = Block {
  blockLabel :: LabelRef,
  blockInstructions :: [Instruction]
} deriving (Eq)

instance Show Block where
  show (Block label instructions) = show label ++ ":" ++
    if listEmpty instructions then "" else "\n" ++ intercalate "\n" (map (\i -> "  " ++ show i) instructions)
    where
      listEmpty [] = True
      listEmpty _ = False

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

type VarRef = AST.Symbol

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
  | Jmp LabelRef
  | JmpIf Value LabelRef LabelRef
  | Phi RegName [(LabelRef, RegName)]
  deriving (Eq)

instance Show Instruction where
  show (Ret v) = "ret " ++ maybe "" show v
  show (Set v1 v2) = "set " ++ show v1 ++ " " ++ show v2
  show (BinOp op v1 v2 v3) = show op ++ " " ++ show v1 ++ " " ++ show v2 ++ " " ++ show v3
  show (Jmp l) = "jmp " ++ show l
  show (JmpIf v l1 l2) = "jmpif " ++ show v ++ " " ++ show l1 ++ " " ++ show l2
  show (Phi reg pairs) = "phi " ++ show reg ++ " " ++ showPairs pairs
    where
      showPairs = unwords . map (\(l, r) -> show l ++ " " ++ show r)

data BinOpType
  = Add | Sub | Mul | Div | Eq | Lt | Gt | Le | Ge | Ne
  deriving (Eq)

instance Show BinOpType where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Eq = "=="
  show Lt = "<"
  show Gt = ">"
  show Le = "<="
  show Ge = ">="
  show Ne = "!="

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

data RegEntry a
  = Virtual RegName
  | Physical a
  deriving (Show, Eq, Ord)