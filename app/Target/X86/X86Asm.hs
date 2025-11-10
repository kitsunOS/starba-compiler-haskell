module Target.X86.X86Asm where

import qualified Backend.Reg.Register as Register

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
  | Cmp Operand Operand
  | Push Register32
  | Pop Register32
  | Jmp Label
  | Je Label
  | Sete Register8
  | Setne Register8
  | Setl Register8
  | Setle Register8
  | Setg Register8
  | Setge Register8
  | Movzx Register32 Register8
  | Neg Register32
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

data Register8 = AL | CL | DL | BL | AH | CH | DH | BH
  deriving (Show, Eq, Ord)

instance Register.Register Register8 where
  formatName AL = "al"
  formatName CL = "cl"
  formatName DL = "dl"
  formatName BL = "bl"
  formatName AH = "ah"
  formatName CH = "ch"
  formatName DH = "dh"
  formatName BH = "bh"

data Register32 = ESP | EBP | ESI | EDI | EAX | EBX | ECX | EDX
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

reg8To32 :: Register8 -> Register32
reg8To32 AL = EAX
reg8To32 BL = EBX
reg8To32 CL = ECX
reg8To32 DL = EDX
reg8To32 AH = EAX
reg8To32 BH = EBX
reg8To32 CH = ECX
reg8To32 DH = EDX

reg32To8 :: Register32 -> Register8
reg32To8 EAX = AL
reg32To8 EBX = BL
reg32To8 ECX = CL
reg32To8 EDX = DL
reg32To8 x = error $ "Invalid 32-bit register for 8-bit conversion " ++ show x