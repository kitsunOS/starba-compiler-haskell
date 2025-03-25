module X86.X86Asm where

newtype X86Module = X86Module [Section]

data Section = Section String [LabelledBlock]
  deriving (Show, Eq)

data LabelledBlock = LabelledBlock Label [Instr]
  deriving (Show, Eq)

newtype Label = Label String
  deriving (Show, Eq)

data Instr
  = Mov Operand Operand
  | Db Literal
  | Add Operand Operand
  | Sub Operand Operand
  | Mul Operand
  | Div Operand
  deriving (Show, Eq)

data Operand
  = Register Register32
  | Immediate Integer
  | Memory Integer
  | LabelRef Label
  deriving (Show, Eq)

data Literal
  = StringLiteral String
  | IntLiteral Int
  deriving (Show, Eq)

data Register32 = EAX | EBX | ECX | EDX
  deriving (Show, Eq)