-- | The intermediate representation: three-address code, or /quadruples/,
-- grouped into basic blocks.
--
-- A basic block is a straight-line sequence of quads ended by exactly one
-- terminator, which names every block control can continue in. The blocks of
-- a function therefore form its control-flow graph, independently of the
-- order in which they are eventually laid out.
module Latte.Quadruples
  ( -- * Values
    Var (..)
  , Operand (..)
  , Mem (..)
  , operandType
  , memType
    -- * Code
  , Quad (..)
  , ArithOp (..)
  , Callee (..)
  , Support (..)
  , Terminator (..)
  , Comparison (..)
  , negateComparison
  , Label (..)
  , BasicBlock (..)
  , Function (..)
    -- * Data flow
  , definition
  , uses
  , terminatorUses
  , successors
  ) where

import Latte.Core (ArithOp (..), Comparison (..), Var (..), negateComparison)
import Latte.Layout (Field (..))
import Latte.Types

-- | A value a quad reads: a variable of the program, a temporary introduced
-- by the compiler, or a constant.
data Operand
  = Ref Var
  | Const Type Integer
    -- ^ A constant. Booleans are 0 and 1; the null reference is 0.
  deriving (Eq, Show)

-- | A location on the heap.
data Mem
  = Member Var Field          -- ^ A field of an object.
  | Element Var Operand Type  -- ^ An element of an array, and its type.
  | Length Var                -- ^ The length of an array.
  deriving (Eq, Show)

operandType :: Operand -> Type
operandType (Ref var)    = varType var
operandType (Const ty _) = ty

-- | The type of the value at a location.
memType :: Mem -> Type
memType (Member _ field) = fieldType field
memType (Element _ _ ty) = ty
memType (Length _)       = TInt

data Quad
  = Assign Var Operand                 -- ^ @x := y@
  | LoadString Var String              -- ^ @x := "literal"@
  | BinOp Var ArithOp Operand Operand  -- ^ @x := y op z@
  | Negate Var Operand                 -- ^ @x := -y@
  | Load Var Mem                       -- ^ @x := [m]@
  | Store Mem Operand                  -- ^ @[m] := x@
  | Bump Mem Integer                   -- ^ @[m] := [m] + n@
  | Call (Maybe Var) Callee [Operand]  -- ^ @x := f(y, ...)@, or just @f(y, ...)@
  deriving (Eq, Show)

data Callee
  = Defined String   -- ^ A function of the program being compiled.
  | Builtin Builtin  -- ^ A built-in function of the language.
  | Support Support  -- ^ A function of the runtime library that only compiled code calls.
  deriving (Eq, Show)

data Support
  = Concat        -- ^ Concatenation of two strings.
  | StringEquals  -- ^ Comparison of two strings by content.
  | NewObject     -- ^ Allocation of a zeroed object of the given size in bytes.
  | NewArray      -- ^ Allocation of a zeroed array, given its length and element size.
  deriving (Eq, Show)

data Terminator
  = Goto Label
  | Branch Comparison Operand Operand Label Label
    -- ^ Continues in the first block if the comparison holds, else in the second.
  | Return (Maybe Operand)
  deriving (Eq, Show)

-- | The name of a basic block, unique in the whole program.
newtype Label = Label Int
  deriving (Eq, Ord, Show)

data BasicBlock = BasicBlock
  { blockLabel :: Label
  , blockQuads :: [Quad]
  , blockEnd   :: Terminator
  }
  deriving (Eq, Show)

data Function = Function
  { functionName   :: String
  , functionParams :: [Var]
  , functionBlocks :: [BasicBlock]  -- ^ The entry block comes first.
  }
  deriving (Eq, Show)

-- | The variable a quad assigns, if any.
definition :: Quad -> Maybe Var
definition quad = case quad of
  Assign target _     -> Just target
  LoadString target _ -> Just target
  BinOp target _ _ _  -> Just target
  Negate target _     -> Just target
  Load target _       -> Just target
  Store _ _           -> Nothing
  Bump _ _            -> Nothing
  Call target _ _     -> target

-- | The variables a quad reads.
uses :: Quad -> [Var]
uses quad = case quad of
  Assign _ source      -> operandVars source
  LoadString _ _       -> []
  BinOp _ _ left right -> operandVars left ++ operandVars right
  Negate _ operand     -> operandVars operand
  Load _ memory        -> memVars memory
  Store memory value   -> memVars memory ++ operandVars value
  Bump memory _        -> memVars memory
  Call _ _ args        -> concatMap operandVars args

terminatorUses :: Terminator -> [Var]
terminatorUses end = case end of
  Goto _                  -> []
  Branch _ left right _ _ -> operandVars left ++ operandVars right
  Return value            -> maybe [] operandVars value

-- | The blocks control can continue in.
successors :: Terminator -> [Label]
successors end = case end of
  Goto target                 -> [target]
  Branch _ _ _ onTrue onFalse -> [onTrue, onFalse]
  Return _                    -> []

operandVars :: Operand -> [Var]
operandVars (Ref var)   = [var]
operandVars (Const _ _) = []

memVars :: Mem -> [Var]
memVars memory = case memory of
  Member object _       -> [object]
  Element array index _ -> array : operandVars index
  Length array          -> [array]
