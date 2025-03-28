module X86.X86Asm where
import qualified Register

newtype X86Module = X86Module [Section]

data Section = Section String [LabelledBlock]
  deriving (Show, Eq)

data LabelledBlock = LabelledBlock {
  blockLabel :: Label,
  blockInsts :: [Instr],
  blockGlobal :: Bool
}
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
  | Push Register32
  | Pop Register32
  | Ret
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

data Register32 = EAX | EBX | ECX | EDX | ESP | EBP | ESI | EDI
  deriving (Show, Eq, Ord)

instance Register.Register Register32 where
  formatName EAX = "eax"
  formatName EBX = "ebx"
  formatName ECX = "ecx"
  formatName EDX = "edx"
  formatName ESP = "esp"
  formatName EBP = "ebp"
  formatName ESI = "esi"
  formatName EDI = "edi"