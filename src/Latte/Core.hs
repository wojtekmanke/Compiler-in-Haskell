-- | The core language: a Latte program as the type checker understands it.
--
-- Compared with the syntax tree, nothing is left to be looked up or inferred.
-- Every variable is identified by a number, so scopes no longer matter; every
-- field access knows the field it refers to and every call its target; and
-- operations that the syntax overloads, such as @+@ and @==@, are told apart
-- by the type of their operands. The later passes rely on this and never
-- have to consult the declarations of the program again.
module Latte.Core
  ( Program (..)
  , Function (..)
  , Var (..)
  , Stmt (..)
  , Location (..)
  , Expr (..)
  , Callee (..)
  , ArithOp (..)
  , Comparison (..)
  , negateComparison
  ) where

import Latte.Error (Position)
import Latte.Layout (Field)
import Latte.Types

data Program = Program
  { programFunctions :: [Function]
  , programVariables :: Int  -- ^ The number of variables; they are numbered from 0.
  }

data Function = Function
  { functionName     :: String
  , functionPosition :: Position
  , functionReturn   :: Type
  , functionParams   :: [Var]
  , functionBody     :: [Stmt]
  }

-- | A variable, identified by a number that is unique in the program.
data Var = Var
  { varId   :: Int
  , varType :: Type
  }
  deriving (Eq, Ord, Show)

-- | Statements. A block is simply a list of them: with variables resolved,
-- nesting carries no information.
data Stmt
  = Declare Var (Maybe Expr)
    -- ^ Without an initialiser, the variable gets the default value of its type.
  | Assign Location Expr
  | Bump Location Integer
    -- ^ Adds a constant to an @int@: @++@ and @--@.
  | Return (Maybe Expr)
  | If Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | ForEach Var Expr [Stmt]
    -- ^ Runs the body with the variable set to each element of an array in turn.
  | Evaluate Expr

-- | Something that can be read and assigned to.
data Location
  = Variable Var
  | FieldOf Expr Field
  | ElementOf Expr Expr Type  -- ^ An array, an index, and the type of the elements.

data Expr
  = Read Location
  | Length Expr                -- ^ The length of an array.
  | IntLit Integer
  | BoolLit Bool
  | StringLit String
  | Null String                -- ^ The null reference of a class.
  | New String Int             -- ^ A new object of a class, and its size in bytes.
  | NewArray Type Expr         -- ^ A new array, given the type of its elements and its length.
  | Call Callee Type [Expr]    -- ^ A call, with the type of its result.
  | Negate Expr
  | Not Expr
  | Arithmetic ArithOp Expr Expr
  | Concat Expr Expr           -- ^ Concatenation of strings.
  | Compare Comparison Expr Expr
    -- ^ Comparison of integers and, for equality only, of booleans and references.
  | StringEquals Expr Expr     -- ^ Comparison of strings by content.
  | And Expr Expr
  | Or Expr Expr

data Callee
  = Defined String   -- ^ A function of the program.
  | Builtin Builtin  -- ^ A built-in function.
  deriving (Eq, Show)

data ArithOp = Add | Subtract | Multiply | Divide | Remainder
  deriving (Eq, Show)

data Comparison = Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual
  deriving (Eq, Show)

negateComparison :: Comparison -> Comparison
negateComparison comparison = case comparison of
  Less         -> GreaterEqual
  LessEqual    -> Greater
  Greater      -> LessEqual
  GreaterEqual -> Less
  Equal        -> NotEqual
  NotEqual     -> Equal
