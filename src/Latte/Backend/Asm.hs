-- | A small model of x86-64 assembly in AT&T syntax, as read by GNU as.
module Latte.Backend.Asm
  ( Reg (..)
  , Arg (..)
  , Instruction (..)
  , Line (..)
  , jumpTarget
  , render
  , stringLiteral
  ) where

import Data.Bits (shiftR, (.&.))
import Data.Char (isAscii, isPrint, ord)
import Data.List (intercalate)
import Text.Printf (printf)

import Latte.Core (Comparison (..))
import Latte.Layout (Width (..))

data Reg = RAX | RCX | RDX | RSI | RDI | R8 | R9 | R10 | R11 | RBP | RSP
  deriving (Eq, Ord, Show)

-- | An operand of an instruction. A register is named without its size,
-- which follows from the instruction it is used in.
data Arg
  = Imm Integer
  | Register Reg
  | Memory Int Reg           -- ^ @displacement(base)@
  | Indexed Int Reg Reg Int  -- ^ @displacement(base, index, scale)@
  deriving (Eq, Show)

-- | The instructions the compiler emits. Operands are in AT&T order, with
-- the destination last.
data Instruction
  = Mov Width Arg Arg
  | Movslq Reg                -- ^ Sign-extends the lower half of a register to all of it.
  | Lea String Reg            -- ^ Loads the address of a label.
  | Add Width Arg Arg
  | Sub Width Arg Arg
  | Imul Width Arg Arg
  | Neg Width Arg
  | Cltd                      -- ^ Sign-extends @%eax@ into @%edx@, as @idiv@ expects.
  | Idiv Width Arg
  | Cmp Width Arg Arg         -- ^ Compares the second operand with the first.
  | Jmp String
  | Jcc Comparison String     -- ^ Jumps if the last comparison came out as given.
  | Call String
  | Push Arg
  | Leave
  | Ret
  deriving (Eq, Show)

data Line
  = LabelDef String
  | Code Instruction
  | Directive String
  | Blank
  deriving (Eq, Show)

-- | The label a line jumps to, if it is a jump.
jumpTarget :: Line -> Maybe String
jumpTarget line = case line of
  Code (Jmp target)   -> Just target
  Code (Jcc _ target) -> Just target
  _                   -> Nothing

render :: [Line] -> String
render = unlines . map renderLine

renderLine :: Line -> String
renderLine line = case line of
  LabelDef name    -> name ++ ":"
  Code instruction -> '\t' : renderInstruction instruction
  Directive text   -> '\t' : text
  Blank            -> ""

renderInstruction :: Instruction -> String
renderInstruction instruction = case instruction of
  Mov width source target  -> sized "mov" width [source, target]
  Movslq reg               -> plain "movslq" [register W32 reg, register W64 reg]
  Lea label reg            -> plain "leaq" [label ++ "(%rip)", register W64 reg]
  Add width source target  -> sized "add" width [source, target]
  Sub width source target  -> sized "sub" width [source, target]
  Imul width source target -> sized "imul" width [source, target]
  Neg width target         -> sized "neg" width [target]
  Cltd                     -> plain "cltd" []
  Idiv width divisor       -> sized "idiv" width [divisor]
  Cmp width right left     -> sized "cmp" width [right, left]
  Jmp target               -> plain "jmp" [target]
  Jcc comparison target    -> plain (conditionalJump comparison) [target]
  Call symbol              -> plain "call" [symbol]
  Push value               -> sized "push" W64 [value]
  Leave                    -> plain "leave" []
  Ret                      -> plain "ret" []
  where
    sized mnemonic width args = plain (mnemonic ++ suffix width) (map (renderArg width) args)
    plain mnemonic []   = mnemonic
    plain mnemonic args = mnemonic ++ "\t" ++ intercalate ", " args

-- | The mnemonic suffix that selects the operand size of an instruction.
suffix :: Width -> String
suffix W32 = "l"
suffix W64 = "q"

conditionalJump :: Comparison -> String
conditionalJump comparison = case comparison of
  Less         -> "jl"
  LessEqual    -> "jle"
  Greater      -> "jg"
  GreaterEqual -> "jge"
  Equal        -> "je"
  NotEqual     -> "jne"

-- | Renders an operand of an instruction that works on values of the given
-- size. Addresses are always 64 bits wide.
renderArg :: Width -> Arg -> String
renderArg width arg = case arg of
  Imm value -> "$" ++ show value
  Register reg -> register width reg
  Memory displacement base -> offset displacement ++ "(" ++ register W64 base ++ ")"
  Indexed displacement base index scale ->
    offset displacement ++ "(" ++ register W64 base ++ ", " ++ register W64 index
      ++ ", " ++ show scale ++ ")"
  where
    offset 0 = ""
    offset displacement = show displacement

register :: Width -> Reg -> String
register width reg = '%' : case (reg, width) of
  (RAX, W64) -> "rax"
  (RAX, W32) -> "eax"
  (RCX, W64) -> "rcx"
  (RCX, W32) -> "ecx"
  (RDX, W64) -> "rdx"
  (RDX, W32) -> "edx"
  (RSI, W64) -> "rsi"
  (RSI, W32) -> "esi"
  (RDI, W64) -> "rdi"
  (RDI, W32) -> "edi"
  (R8, W64)  -> "r8"
  (R8, W32)  -> "r8d"
  (R9, W64)  -> "r9"
  (R9, W32)  -> "r9d"
  (R10, W64) -> "r10"
  (R10, W32) -> "r10d"
  (R11, W64) -> "r11"
  (R11, W32) -> "r11d"
  (RBP, W64) -> "rbp"
  (RBP, W32) -> "ebp"
  (RSP, W64) -> "rsp"
  (RSP, W32) -> "esp"

-- | A directive defining a NUL-terminated string. Everything except printable
-- ASCII is written as octal escapes of its UTF-8 encoding, which keeps the
-- output independent of the encoding the assembler assumes.
stringLiteral :: String -> Line
stringLiteral text = Directive (".string\t\"" ++ concatMap escape text ++ "\"")
  where
    escape char
      | char == '"' || char == '\\'  = ['\\', char]
      | isAscii char && isPrint char = [char]
      | otherwise                    = concatMap (printf "\\%03o") (utf8 (ord char))

-- | The UTF-8 encoding of a code point.
utf8 :: Int -> [Int]
utf8 code
  | code < 0x80    = [code]
  | code < 0x800   = [0xC0 + code `shiftR` 6, continuation code]
  | code < 0x10000 = [0xE0 + code `shiftR` 12, continuation (code `shiftR` 6), continuation code]
  | otherwise      =
      [ 0xF0 + code `shiftR` 18
      , continuation (code `shiftR` 12)
      , continuation (code `shiftR` 6)
      , continuation code
      ]
  where
    continuation bits = 0x80 + bits .&. 0x3F
