-- | Constant folding.
--
-- Expressions whose value is known at compile time are replaced by that
-- value, and branches and loops whose condition folds to a constant are
-- simplified. Folding gives exactly the result the generated code would
-- compute: arithmetic wraps to 32 bits and division truncates towards zero.
module Latte.Frontend.ConstFold (foldStmts) where

import Data.Int (Int32)

import Latte.Core

foldStmts :: [Stmt] -> [Stmt]
foldStmts = concatMap foldStmt

-- | Folds a statement, which may leave several statements, or none.
foldStmt :: Stmt -> [Stmt]
foldStmt stmt = case stmt of
  Declare var initial -> [Declare var (foldExpr <$> initial)]
  Assign target value -> [Assign (foldLocation target) (foldExpr value)]
  Bump target amount -> [Bump (foldLocation target) amount]
  Return value -> [Return (foldExpr <$> value)]
  If condition whenTrue whenFalse -> case foldExpr condition of
    BoolLit True  -> foldStmts whenTrue
    BoolLit False -> foldStmts whenFalse
    condition'    -> [If condition' (foldStmts whenTrue) (foldStmts whenFalse)]
  While condition body -> case foldExpr condition of
    BoolLit False -> []
    condition'    -> [While condition' (foldStmts body)]
  ForEach var array body -> [ForEach var (foldExpr array) (foldStmts body)]
  Evaluate expr -> [Evaluate (foldExpr expr)]

foldLocation :: Location -> Location
foldLocation location = case location of
  Variable _ -> location
  FieldOf object field -> FieldOf (foldExpr object) field
  ElementOf array index element -> ElementOf (foldExpr array) (foldExpr index) element

foldExpr :: Expr -> Expr
foldExpr expr = case expr of
  Read location -> Read (foldLocation location)
  Length array -> Length (foldExpr array)
  IntLit _ -> expr
  BoolLit _ -> expr
  StringLit _ -> expr
  Null _ -> expr
  New _ _ -> expr
  NewArray element size -> NewArray element (foldExpr size)
  Call callee result args -> Call callee result (map foldExpr args)
  Negate operand -> case foldExpr operand of
    IntLit value -> IntLit (wrap (negate value))
    operand'     -> Negate operand'
  Not operand -> case foldExpr operand of
    BoolLit value -> BoolLit (not value)
    operand'      -> Not operand'
  Arithmetic op left right -> case (foldExpr left, foldExpr right) of
    (IntLit x, IntLit y) | Just value <- evaluate op x y -> IntLit value
    (left', right') -> Arithmetic op left' right'
  Concat left right -> case (foldExpr left, foldExpr right) of
    (StringLit x, StringLit y) -> StringLit (x ++ y)
    (left', right') -> Concat left' right'
  Compare comparison left right -> case (foldExpr left, foldExpr right) of
    (IntLit x, IntLit y) -> BoolLit (holds comparison x y)
    (left', right') -> Compare comparison left' right'
  StringEquals left right -> case (foldExpr left, foldExpr right) of
    (StringLit x, StringLit y) -> BoolLit (x == y)
    (left', right') -> StringEquals left' right'
  -- An operand may only be dropped if it is not evaluated at run time, or is
  -- a constant: `f() && false` still has to call `f`.
  And left right -> case (foldExpr left, foldExpr right) of
    (BoolLit False, _)     -> BoolLit False
    (BoolLit True, right') -> right'
    (left', BoolLit True)  -> left'
    (left', right')        -> And left' right'
  Or left right -> case (foldExpr left, foldExpr right) of
    (BoolLit True, _)       -> BoolLit True
    (BoolLit False, right') -> right'
    (left', BoolLit False)  -> left'
    (left', right')         -> Or left' right'

-- | Reduces an integer to the range of a 32-bit @int@, as machine arithmetic does.
wrap :: Integer -> Integer
wrap value = toInteger (fromInteger value :: Int32)

-- | The value of an arithmetic operation. A division is left unfolded where
-- the processor would trap, so that the program fails at run time exactly as
-- it would without folding.
evaluate :: ArithOp -> Integer -> Integer -> Maybe Integer
evaluate op x y = case op of
  Add       -> Just (wrap (x + y))
  Subtract  -> Just (wrap (x - y))
  Multiply  -> Just (wrap (x * y))
  Divide    | traps     -> Nothing
            | otherwise -> Just (x `quot` y)
  Remainder | traps     -> Nothing
            | otherwise -> Just (x `rem` y)
  where
    traps = y == 0 || (x == toInteger (minBound :: Int32) && y == -1)

holds :: Comparison -> Integer -> Integer -> Bool
holds comparison x y = case comparison of
  Less         -> x < y
  LessEqual    -> x <= y
  Greater      -> x > y
  GreaterEqual -> x >= y
  Equal        -> x == y
  NotEqual     -> x /= y
