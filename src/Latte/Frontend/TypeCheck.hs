{-# LANGUAGE LambdaCase #-}

-- | Name resolution and type checking.
--
-- The checker does more than accept or reject a program: it translates the
-- syntax tree into the core language of "Latte.Core", in which every name is
-- resolved and every operation knows the types it works on.
--
-- Names follow three rules, the last two of which are stricter than in plain
-- Latte and were chosen to match Java:
--
--   * a variable cannot be declared twice in one block, but may hide a
--     variable of an enclosing block;
--   * a variable cannot hide a parameter of its function;
--   * variables, functions and classes share a single namespace.
module Latte.Frontend.TypeCheck (typeCheck) where

import Control.Monad (forM, forM_, unless, when, zipWithM)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.Reader (ReaderT, ask, asks, local, runReaderT)
import Control.Monad.State (StateT, runStateT, state)
import Data.Int (Int32)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Latte.Abs hiding (Type)
import qualified Latte.Abs as Abs
import qualified Latte.Core as Core
import Latte.Error
import Latte.Layout
import Latte.Types

-- | The checker reads its environment, numbers the variables it meets, and
-- stops at the first error.
type Check = ReaderT Env (StateT Int (Either CompileError))

data Env = Env
  { envClasses   :: Map.Map String Layout
  , envFunctions :: Map.Map String (Core.Callee, FunType)
  , envReturn    :: Type                     -- ^ Return type of the enclosing function.
  , envVars      :: Map.Map String Core.Var  -- ^ The variables in scope.
  , envBlock     :: Set.Set String           -- ^ Variables declared in the innermost block.
  , envParams    :: Set.Set String           -- ^ Parameters of the enclosing function.
  }

-- | Checks a program and translates it to the core language.
typeCheck :: Program -> Either CompileError Core.Program
typeCheck (Program _ definitions) = do
  checkDistinct $
    [(Nothing, name, "a built-in function") | (name, _, _) <- builtins]
      ++ map describe definitions
  classes <- sequence
    [checkClass classNames name members | ClDef _ name members <- definitions]
  signatures <- sequence
    [checkSignature classNames ret name args | FnDef _ ret name args _ <- definitions]
  checkMain definitions
  let env = Env
        { envClasses   = Map.fromList classes
        , envFunctions = Map.fromList $
            [(name, (Core.Builtin builtin, ty)) | (name, builtin, ty) <- builtins]
              ++ [(name, (Core.Defined name, ty)) | (name, ty) <- signatures]
        , envReturn    = TVoid
        , envVars      = Map.empty
        , envBlock     = Set.empty
        , envParams    = Set.empty
        }
      check = sequence
        [checkFunction pos ret name args body | FnDef pos ret name args body <- definitions]
  (functions, variables) <- runStateT (runReaderT check env) 0
  return (Core.Program functions variables)
  where
    classNames = Set.fromList [identName name | ClDef _ name _ <- definitions]
    describe (FnDef pos _ name _ _) = (pos, identName name, "a function")
    describe (ClDef pos name _)     = (pos, identName name, "a class")

-- * Definitions

checkClass :: Set.Set String -> Ident -> [ClassEl] -> Either CompileError (String, Layout)
checkClass classNames name members = do
  checkDistinct [(pos, identName field, "a field") | ClElem pos _ field <- members]
  fields <- forM members $ \(ClElem _ ty field) -> do
    fieldTy <- valueType classNames ty
    return (identName field, fieldTy)
  return (identName name, layout fields)

checkSignature
  :: Set.Set String -> Abs.Type -> Ident -> [Arg] -> Either CompileError (String, FunType)
checkSignature classNames ret name args = do
  checkDistinct [(pos, identName arg, "a parameter") | Arg pos _ arg <- args]
  result <- resolveType classNames ret
  params <- forM args $ \(Arg _ ty _) -> valueType classNames ty
  return (identName name, FunType result params)

checkMain :: [TopDef] -> Either CompileError ()
checkMain definitions =
  case [(pos, ret, args) | FnDef pos ret (Ident "main") args _ <- definitions] of
    [] -> Left (CompileError Nothing "the program has no `main` function")
    (pos, ret, args) : _ ->
      unless (fromSyntax ret == TInt && null args) $
        Left (CompileError pos "`main` must be declared as `int main()`")

checkFunction :: Position -> Abs.Type -> Ident -> [Arg] -> Block -> Check Core.Function
checkFunction pos ret name args (Block _ body) = do
  forM_ args $ \(Arg argPos _ arg) -> checkNotGlobal argPos (identName arg)
  params <- forM args $ \(Arg _ ty arg) -> (,) (identName arg) <$> fresh (fromSyntax ty)
  let enter env = env
        { envReturn = fromSyntax ret
        , envVars   = Map.fromList params
        , envParams = Set.fromList (map fst params)
        }
  statements <- local enter (checkBlock body)
  return Core.Function
    { Core.functionName     = identName name
    , Core.functionPosition = pos
    , Core.functionReturn   = fromSyntax ret
    , Core.functionParams   = map snd params
    , Core.functionBody     = statements
    }

-- * Statements

-- | Checks the statements of one block, which form a scope of their own.
checkBlock :: [Stmt] -> Check [Core.Stmt]
checkBlock = local (\env -> env { envBlock = Set.empty }) . go
  where
    go [] = return []
    go (Decl _ ty items : rest) = do
      declared <- resolve valueType ty
      declare declared items (go rest)
    go (stmt : rest) = (++) <$> checkStmt stmt <*> go rest

-- | Declares variables and checks the given rest of their block, in which
-- they are in scope.
declare :: Type -> [Item] -> Check [Core.Stmt] -> Check [Core.Stmt]
declare _ [] continue = continue
declare ty (item : items) continue = do
  (pos, name, initial) <- case item of
    NoInit pos name -> return (pos, name, Nothing)
    Init pos name value -> do
      -- A variable is not in scope in its own initialiser.
      initial <- expect ty value
      return (pos, name, Just initial)
  withVar pos name ty $ \var ->
    (Core.Declare var initial :) <$> declare ty items continue

checkStmt :: Stmt -> Check [Core.Stmt]
checkStmt = \case
  Empty _ -> return []
  BStmt _ (Block _ stmts) -> checkBlock stmts
  -- A declaration that is the whole body of an `if` or of a loop.
  stmt@Decl{} -> checkBlock [stmt]
  Ass _ target value -> do
    (ty, location) <- checkAssignable target
    result <- expect ty value
    return [Core.Assign location result]
  Incr _ target -> checkCounter target 1
  Decr _ target -> checkCounter target (-1)
  Ret pos value -> do
    expected <- asks envReturn
    when (expected == TVoid) $ throwAt pos "a `void` function cannot return a value"
    (actual, result) <- inferExpr value
    unless (actual == expected) $
      throwAt pos $ "the function returns " ++ quote expected
        ++ ", but this value has type " ++ quote actual
    return [Core.Return (Just result)]
  VRet pos -> do
    expected <- asks envReturn
    unless (expected == TVoid) $
      throwAt pos $ "the function must return a value of type " ++ quote expected
    return [Core.Return Nothing]
  Cond _ condition body -> do
    test <- expect TBool condition
    whenTrue <- checkStmt body
    return [Core.If test whenTrue []]
  CondElse _ condition bodyTrue bodyFalse -> do
    test <- expect TBool condition
    whenTrue <- checkStmt bodyTrue
    whenFalse <- checkStmt bodyFalse
    return [Core.If test whenTrue whenFalse]
  While _ condition body -> do
    test <- expect TBool condition
    statements <- checkStmt body
    return [Core.While test statements]
  For pos ty name array body -> do
    element <- resolve valueType ty
    elements <- expect (TArray element) array
    local (\env -> env { envBlock = Set.empty }) $
      withVar pos name element $ \var -> do
        statements <- checkStmt body
        return [Core.ForEach var elements statements]
  SExp _ expr -> do
    (_, result) <- inferExpr expr
    return [Core.Evaluate result]

-- | Checks that an expression denotes a variable, a field or an array
-- element, and returns the type of the values it can hold.
checkAssignable :: Expr -> Check (Type, Core.Location)
checkAssignable target = do
  (ty, expr) <- inferExpr target
  case expr of
    Core.Read location -> return (ty, location)
    Core.Length _ -> throwAt (hasPosition target) "the length of an array cannot be changed"
    _ -> throwAt (hasPosition target) "this expression cannot be assigned to"

checkCounter :: Expr -> Integer -> Check [Core.Stmt]
checkCounter target amount = do
  (ty, location) <- checkAssignable target
  unless (ty == TInt) $
    throwAt (hasPosition target) $
      "`++` and `--` need an operand of type `int`, found " ++ quote ty
  return [Core.Bump location amount]

-- * Expressions

-- | Checks that an expression has the given type.
expect :: Type -> Expr -> Check Core.Expr
expect expected expr = do
  (actual, result) <- inferExpr expr
  unless (actual == expected) $
    throwAt (hasPosition expr) $
      "expected an expression of type " ++ quote expected ++ ", found " ++ quote actual
  return result

inferExpr :: Expr -> Check (Type, Core.Expr)
inferExpr = \case
  EVar pos (Ident name) -> do
    vars <- asks envVars
    case Map.lookup name vars of
      Just var -> return (Core.varType var, Core.Read (Core.Variable var))
      Nothing  -> throwAt pos ("unknown variable `" ++ name ++ "`")
  ELitInt pos value -> do
    checkLiteral pos value maxInt
    return (TInt, Core.IntLit value)
  ELitTrue _ -> return (TBool, Core.BoolLit True)
  ELitFalse _ -> return (TBool, Core.BoolLit False)
  EString _ text -> return (TString, Core.StringLit text)
  ENull pos castTo -> case castTo of
    EVar _ (Ident name) -> do
      _ <- classLayout pos name
      return (TClass name, Core.Null name)
    _ -> throwAt pos "`null` must be cast to a class, as in `(C)null`"
  ENew pos ty -> case ty of
    ClassT _ (Ident name) -> do
      fields <- classLayout pos name
      return (TClass name, Core.New name (layoutSize fields))
    _ -> throwAt pos "`new` needs a class; an array is created with `new T[size]`"
  ENewArray _ ty size -> do
    element <- resolve valueType ty
    len <- expect TInt size
    return (TArray element, Core.NewArray element len)
  EApp pos (Ident name) args -> do
    functions <- asks envFunctions
    (callee, FunType result params) <-
      maybe (throwAt pos ("unknown function `" ++ name ++ "`")) return (Map.lookup name functions)
    unless (length args == length params) $
      throwAt pos $ "function `" ++ name ++ "` takes " ++ show (length params)
        ++ " argument(s), but " ++ show (length args) ++ " were given"
    arguments <- zipWithM expect params args
    return (result, Core.Call callee result arguments)
  EField pos object (Ident name) -> do
    (objectType, base) <- inferExpr object
    let missing = throwAt pos (quote objectType ++ " has no field `" ++ name ++ "`")
    case objectType of
      TArray _ | name == "length" -> return (TInt, Core.Length base)
      TClass cls -> do
        fields <- layoutFields <$> classLayout pos cls
        case Map.lookup name fields of
          Just field -> return (fieldType field, Core.Read (Core.FieldOf base field))
          Nothing    -> missing
      _ -> missing
  EArrayItem pos array index -> do
    (arrayType, base) <- inferExpr array
    position <- expect TInt index
    case arrayType of
      TArray element -> return (element, Core.Read (Core.ElementOf base position element))
      _ -> throwAt pos ("only arrays can be indexed, found " ++ quote arrayType)
  -- A negated literal is a constant in its own right: the magnitude of the
  -- smallest `int` is not itself an `int`.
  Neg _ (ELitInt pos value) -> do
    checkLiteral pos value (maxInt + 1)
    return (TInt, Core.IntLit (negate value))
  Neg _ operand -> do
    value <- expect TInt operand
    return (TInt, Core.Negate value)
  Not _ operand -> do
    value <- expect TBool operand
    return (TBool, Core.Not value)
  EMul _ left op right -> arithmetic left right $ case op of
    Times _ -> Core.Multiply
    Div _   -> Core.Divide
    Mod _   -> Core.Remainder
  EAdd pos left (Plus _) right -> do
    (ty, x) <- inferExpr left
    unless (ty == TInt || ty == TString) $
      throwAt pos ("`+` needs operands of type `int` or `string`, found " ++ quote ty)
    y <- expect ty right
    return (ty, if ty == TString then Core.Concat x y else Core.Arithmetic Core.Add x y)
  EAdd _ left (Minus _) right -> arithmetic left right Core.Subtract
  ERel pos left op right -> case op of
    LTH _ -> ordering Core.Less
    LE _  -> ordering Core.LessEqual
    GTH _ -> ordering Core.Greater
    GE _  -> ordering Core.GreaterEqual
    EQU _ -> equality Core.Equal id
    NE _  -> equality Core.NotEqual Core.Not
    where
      ordering comparison = do
        x <- expect TInt left
        y <- expect TInt right
        return (TBool, Core.Compare comparison x y)
      -- Strings are compared by content, everything else directly.
      equality comparison ofStrings = do
        (ty, x) <- inferExpr left
        when (ty == TVoid) $ throwAt pos "a `void` expression has no value to compare"
        y <- expect ty right
        return $ (,) TBool $
          if ty == TString then ofStrings (Core.StringEquals x y) else Core.Compare comparison x y
  EAnd _ left right -> do
    x <- expect TBool left
    y <- expect TBool right
    return (TBool, Core.And x y)
  EOr _ left right -> do
    x <- expect TBool left
    y <- expect TBool right
    return (TBool, Core.Or x y)
  where
    arithmetic left right op = do
      x <- expect TInt left
      y <- expect TInt right
      return (TInt, Core.Arithmetic op x y)

maxInt :: Integer
maxInt = toInteger (maxBound :: Int32)

checkLiteral :: Position -> Integer -> Integer -> Check ()
checkLiteral pos value limit =
  when (value > limit) $
    throwAt pos ("the literal " ++ show value ++ " does not fit in a 32-bit `int`")

-- * Names and types

fresh :: Type -> Check Core.Var
fresh ty = state (\next -> (Core.Var next ty, next + 1))

-- | Declares a variable and runs the given check with it in scope.
withVar :: Position -> Ident -> Type -> (Core.Var -> Check a) -> Check a
withVar pos (Ident name) ty continue = do
  env <- ask
  when (name `Set.member` envBlock env) $
    throwAt pos (alreadyDefined name "a variable in this block")
  when (name `Set.member` envParams env) $
    throwAt pos (alreadyDefined name "a parameter of this function")
  checkNotGlobal pos name
  var <- fresh ty
  let extend scope = scope
        { envVars  = Map.insert name var (envVars scope)
        , envBlock = Set.insert name (envBlock scope)
        }
  local extend (continue var)

-- | Variables and parameters cannot be named after a function or a class.
checkNotGlobal :: Position -> String -> Check ()
checkNotGlobal pos name = do
  env <- ask
  when (name `Map.member` envFunctions env) $
    throwAt pos (alreadyDefined name "a function")
  when (name `Map.member` envClasses env) $
    throwAt pos (alreadyDefined name "a class")

classLayout :: Position -> String -> Check Layout
classLayout pos name = do
  classes <- asks envClasses
  maybe (throwAt pos ("unknown class `" ++ name ++ "`")) return (Map.lookup name classes)

-- | Runs one of the type resolvers below against the classes of the program.
resolve :: (Set.Set String -> Abs.Type -> Either CompileError Type) -> Abs.Type -> Check Type
resolve resolver ty = do
  classNames <- asks (Map.keysSet . envClasses)
  liftEither (resolver classNames ty)

-- | Converts a type written in the program, checking that it is well formed.
resolveType :: Set.Set String -> Abs.Type -> Either CompileError Type
resolveType classNames ty = case ty of
  ClassT pos (Ident name) | name `Set.notMember` classNames ->
    Left (CompileError pos ("unknown class `" ++ name ++ "`"))
  Array _ element -> TArray <$> valueType classNames element
  _ -> return (fromSyntax ty)

-- | Like 'resolveType', for the type of a variable, parameter, field or array
-- element, none of which can be @void@.
valueType :: Set.Set String -> Abs.Type -> Either CompileError Type
valueType classNames ty = do
  resolved <- resolveType classNames ty
  when (resolved == TVoid) $
    Left (CompileError (hasPosition ty) "`void` can only be the return type of a function")
  return resolved

-- | Reports the first name that is defined twice. Each name comes with a
-- description of what it denotes, such as @"a function"@.
checkDistinct :: [(Position, String, String)] -> Either CompileError ()
checkDistinct = go Map.empty
  where
    go _ [] = return ()
    go seen ((pos, name, kind) : rest) = case Map.lookup name seen of
      Just previous -> Left (CompileError pos (alreadyDefined name previous))
      Nothing       -> go (Map.insert name kind seen) rest

alreadyDefined :: String -> String -> String
alreadyDefined name kind = "`" ++ name ++ "` is already defined as " ++ kind

quote :: Type -> String
quote ty = "`" ++ renderType ty ++ "`"

throwAt :: Position -> String -> Check a
throwAt pos message = throwError (CompileError pos message)
