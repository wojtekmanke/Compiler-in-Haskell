{-# LANGUAGE LambdaCase #-}

-- | Translation of the core language to quadruples.
module Latte.Quadruples.Generate (generate) where

import Control.Monad (void)
import Control.Monad.State (State, evalState, gets, modify)
import Data.Bifunctor (second)
import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Latte.Core as Core
import Latte.Layout (bytes, widthOf)
import Latte.Quadruples
import Latte.Types

type Gen = State GenState

data GenState = GenState
  { nextVar   :: Int
  , nextLabel :: Int
  , openBlock :: Maybe (Label, [Quad])
    -- ^ The block being filled, with its quads in reverse. There is none
    -- right after a terminator: until the next label, code is unreachable.
  , finished  :: [BasicBlock]
    -- ^ The completed blocks of the current function, in reverse.
  }

generate :: Core.Program -> [Function]
generate (Core.Program functions variables) = evalState (mapM genFunction functions) GenState
  { nextVar   = variables  -- temporaries are numbered after the variables of the program
  , nextLabel = 0
  , openBlock = Nothing
  , finished  = []
  }

genFunction :: Core.Function -> Gen Function
genFunction function = do
  entry <- freshLabel
  label entry
  mapM_ genStmt (Core.functionBody function)
  -- Control gets here in a `void` function without a final `return`. In any
  -- other function this block is unreachable, or ends in a call of `error`.
  terminate (Return Nothing)
  blocks <- gets (reverse . finished)
  modify (\state -> state { finished = [] })
  return Function
    { functionName   = Core.functionName function
    , functionParams = Core.functionParams function
    , functionBlocks = reachable blocks
    }

-- | Drops the blocks that no path from the entry block leads to.
reachable :: [BasicBlock] -> [BasicBlock]
reachable [] = []
reachable blocks@(entry : _) = filter ((`Set.member` visited) . blockLabel) blocks
  where
    ends = Map.fromList [(blockLabel block, blockEnd block) | block <- blocks]
    visited = visit [blockLabel entry] Set.empty
    visit [] seen = seen
    visit (current : pending) seen
      | current `Set.member` seen = visit pending seen
      | otherwise =
          visit (maybe [] successors (Map.lookup current ends) ++ pending) (Set.insert current seen)

-- * Statements

genStmt :: Core.Stmt -> Gen ()
genStmt = \case
  Core.Declare var initial -> do
    -- Every type defaults to zero: 0, false, the empty string or null.
    value <- maybe (return (Const (varType var) 0)) genExpr initial
    emit (Assign var value)
  Core.Assign target value -> do
    -- The right-hand side is evaluated before the location assigned to.
    result <- genExpr value
    genLocation target >>= \case
      InVar var       -> emit (Assign var result)
      InMemory memory -> emit (Store memory result)
  Core.Bump target amount ->
    genLocation target >>= \case
      InVar var       -> emit (BinOp var Add (Ref var) (Const TInt amount))
      InMemory memory -> emit (Bump memory amount)
  Core.Return value -> do
    result <- traverse genExpr value
    terminate (Return result)
  Core.If condition whenTrue [] -> do
    onTrue <- freshLabel
    done <- freshLabel
    genCond condition onTrue done
    label onTrue
    mapM_ genStmt whenTrue
    label done
  Core.If condition whenTrue whenFalse -> do
    onTrue <- freshLabel
    onFalse <- freshLabel
    done <- freshLabel
    genCond condition onTrue onFalse
    label onTrue
    mapM_ genStmt whenTrue
    terminate (Goto done)
    label onFalse
    mapM_ genStmt whenFalse
    label done
  -- The condition is placed after the body, so that each iteration executes
  -- a single conditional jump.
  Core.While condition body -> do
    start <- freshLabel
    test <- freshLabel
    done <- freshLabel
    terminate (Goto test)
    label start
    mapM_ genStmt body
    label test
    genCond condition start done
    label done
  Core.ForEach var array body -> do
    -- The array is evaluated once, and the loop variable is a copy of the
    -- current element.
    elements <- freshVar (TArray (varType var))
    source <- genExpr array
    emit (Assign elements source)
    len <- load (Length elements)
    index <- freshVar TInt
    emit (Assign index (Const TInt 0))
    start <- freshLabel
    test <- freshLabel
    done <- freshLabel
    terminate (Goto test)
    label start
    emit (Load var (Element elements (Ref index) (varType var)))
    mapM_ genStmt body
    emit (BinOp index Add (Ref index) (Const TInt 1))
    label test
    terminate (Branch Less (Ref index) len start done)
    label done
  -- The result of a call that is a statement is not needed.
  Core.Evaluate (Core.Call callee _ args) -> do
    operands <- mapM genExpr args
    emit (Call Nothing (genCallee callee) operands)
  Core.Evaluate expr -> void (genExpr expr)

-- * Expressions

-- | Translates an expression and returns the operand holding its value.
genExpr :: Core.Expr -> Gen Operand
genExpr expr = case expr of
  Core.Read location ->
    genLocation location >>= \case
      InVar var       -> return (Ref var)
      InMemory memory -> load memory
  Core.Length array -> do
    base <- genExpr array >>= named
    load (Length base)
  Core.IntLit value -> return (Const TInt value)
  Core.BoolLit value -> return (Const TBool (if value then 1 else 0))
  Core.StringLit text -> do
    var <- freshVar TString
    emit (LoadString var text)
    return (Ref var)
  Core.Null name -> return (Const (TClass name) 0)
  Core.New name size -> call (TClass name) (Support NewObject) [Const TInt (toInteger size)]
  Core.NewArray element size -> do
    len <- genExpr size
    call (TArray element) (Support NewArray) [len, Const TInt (toInteger (bytes (widthOf element)))]
  Core.Call callee result args -> do
    operands <- mapM genExpr args
    call result (genCallee callee) operands
  Core.Negate operand -> do
    value <- genExpr operand
    var <- freshVar TInt
    emit (Negate var value)
    return (Ref var)
  Core.Arithmetic op left right -> do
    x <- genExpr left
    y <- genExpr right
    var <- freshVar TInt
    emit (BinOp var op x y)
    return (Ref var)
  Core.Concat left right -> do
    x <- genExpr left
    y <- genExpr right
    call TString (Support Concat) [x, y]
  Core.StringEquals left right -> do
    x <- genExpr left
    y <- genExpr right
    call TBool (Support StringEquals) [x, y]
  Core.Not _ -> genBoolean expr
  Core.Compare{} -> genBoolean expr
  Core.And _ _ -> genBoolean expr
  Core.Or _ _ -> genBoolean expr

genCallee :: Core.Callee -> Callee
genCallee (Core.Defined name)    = Defined name
genCallee (Core.Builtin builtin) = Builtin builtin

-- | Emits a call and returns the temporary that receives its result.
call :: Type -> Callee -> [Operand] -> Gen Operand
call result callee args = do
  var <- freshVar result
  emit (Call (Just var) callee args)
  return (Ref var)

-- | Reads a memory location into a temporary.
load :: Mem -> Gen Operand
load memory = do
  var <- freshVar (memType memory)
  emit (Load var memory)
  return (Ref var)

-- | Where a location of the core language is: in a variable, or on the heap.
data Place = InVar Var | InMemory Mem

genLocation :: Core.Location -> Gen Place
genLocation = \case
  Core.Variable var -> return (InVar var)
  Core.FieldOf object field -> do
    base <- genExpr object >>= named
    return (InMemory (Member base field))
  Core.ElementOf array index element -> do
    base <- genExpr array >>= named
    position <- genExpr index
    return (InMemory (Element base position element))

-- | Names the value of an operand, so that it can serve as the base of a
-- memory location.
named :: Operand -> Gen Var
named (Ref var) = return var
named constant = do
  var <- freshVar (operandType constant)
  emit (Assign var constant)
  return var

-- | Translates a boolean expression used for its value, by assigning a
-- constant on each of the two paths out of the condition.
genBoolean :: Core.Expr -> Gen Operand
genBoolean expr = do
  result <- freshVar TBool
  onTrue <- freshLabel
  onFalse <- freshLabel
  done <- freshLabel
  genCond expr onTrue onFalse
  label onTrue
  emit (Assign result (Const TBool 1))
  terminate (Goto done)
  label onFalse
  emit (Assign result (Const TBool 0))
  label done
  return (Ref result)

-- | Translates a condition to jumping code: control continues at the first
-- label if the condition holds and at the second if it does not. The
-- operands of @&&@ and @||@ are evaluated only as far as needed, which is
-- what gives the operators their short-circuit semantics.
--
-- The current block is left closed; the caller places the two labels.
genCond :: Core.Expr -> Label -> Label -> Gen ()
genCond condition onTrue onFalse = case condition of
  Core.BoolLit True -> terminate (Goto onTrue)
  Core.BoolLit False -> terminate (Goto onFalse)
  Core.Not operand -> genCond operand onFalse onTrue
  Core.And left right -> do
    middle <- freshLabel
    genCond left middle onFalse
    label middle
    genCond right onTrue onFalse
  Core.Or left right -> do
    middle <- freshLabel
    genCond left onTrue middle
    label middle
    genCond right onTrue onFalse
  Core.Compare comparison left right -> do
    x <- genExpr left
    y <- genExpr right
    terminate (Branch comparison x y onTrue onFalse)
  _ -> do
    value <- genExpr condition
    terminate (Branch NotEqual value (Const TBool 0) onTrue onFalse)

-- * Building blocks

-- | Appends a quad to the current block. Unreachable code is dropped.
emit :: Quad -> Gen ()
emit quad = modify $ \state ->
  state { openBlock = second (quad :) <$> openBlock state }

-- | Ends the current block.
terminate :: Terminator -> Gen ()
terminate end = modify $ \state -> case openBlock state of
  Nothing -> state
  Just (name, quads) -> state
    { openBlock = Nothing
    , finished  = BasicBlock name (reverse quads) end : finished state
    }

-- | Starts the block with the given label. A block that is still open
-- continues in the new one.
label :: Label -> Gen ()
label name = do
  terminate (Goto name)
  modify (\state -> state { openBlock = Just (name, []) })

freshVar :: Type -> Gen Var
freshVar ty = do
  number <- gets nextVar
  modify (\state -> state { nextVar = number + 1 })
  return (Var number ty)

freshLabel :: Gen Label
freshLabel = do
  number <- gets nextLabel
  modify (\state -> state { nextLabel = number + 1 })
  return (Label number)
