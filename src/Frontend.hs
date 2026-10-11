module Frontend where

import AbsLatte

import Misc
import TypeCheck

import Data.Maybe as Maybe
import Data.Foldable (foldlM)

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State (StateT, runStateT)

type Store = ()
type Env = [String]
type Frontend a = ReaderT Env (ErrorT String (StateT Store IO)) a

runFrontend :: Frontend a -> IO (Either String a, Store)
runFrontend fe = runStateT (runErrorT (runReaderT fe [])) ()

analyzeProgram :: Program PositionInfo -> Frontend (StructMap, [TopDef PositionInfo])
analyzeProgram (Program _ classes functions) = do
  -- check if classes contain unique variable names
  mapM_ uniqueClassVars classes
  -- check if class names are unique
  let classNames = map getClassName classes
  areElementsUnique "Class " classNames
  -- check if function names are unique
  let functionNames = map getFunctionName functions
  areElementsUnique "Function " ("readInt":"readString":"printInt":"printString":"error":functionNames)
  -- check if declarations are not overriden
  let takenNames = classNames ++ functionNames
  local (const takenNames) (mapM_ checkFunctionDef functions)
  -- static type check
  let (err, _) = runTypeCheck (typeCheck (Program Nothing classes functions))
  case err of
    Left errMsg -> throwError errMsg
    Right _ -> do
      -- replace const expressions with constants
      let optimizedFunctions = map optimizeTopDef functions
      -- check for returns
      mapM_ checkFunctionReturn optimizedFunctions
      return (getClassesMapping classes, optimizedFunctions)

checkFunctionReturn :: TopDef a -> Frontend ()
checkFunctionReturn (FnDef _ (Void _) _ _ _) = return ()
checkFunctionReturn (FnDef _ _ ident _ (Block _ stmts)) = do
  if or (map checkStmtReturn stmts) then return ()
  else
    throwError $ "Function " ++ getIdentString ident ++ " has no return"


checkStmtReturn :: Stmt a -> Bool
checkStmtReturn (Ret _ _) = True
checkStmtReturn (VRet _) = True
checkStmtReturn (CondElse _ _ stmt1 stmt2) = and [checkStmtReturn stmt1, checkStmtReturn stmt2]
checkStmtReturn (BStmt _ (Block _ stmts)) = or (map checkStmtReturn stmts)
checkStmtReturn (While _ (ELitTrue _) stmt) = checkStmtReturn stmt
checkStmtReturn _ = False

optimizeTopDef :: TopDef a -> TopDef a
optimizeTopDef (FnDef pos t ident args (Block pos2 stmts)) = (FnDef pos t ident args (Block pos2 (optimizeCondStmts (optimizeExprInStmts stmts))))

optimizeCondStmts :: [Stmt a] -> [Stmt a]
optimizeCondStmts stmts = map optimizeCondStmt stmts

optimizeCondStmt :: Stmt a -> Stmt a
optimizeCondStmt (BStmt pos1 (Block pos2 stmts)) = (BStmt pos1 (Block pos2 (optimizeCondStmts stmts)))
optimizeCondStmt (Cond pos (ELitTrue _) stmt) = BStmt pos (Block pos [optimizeCondStmt stmt])
optimizeCondStmt (Cond pos (ELitFalse _) stmt) = Empty pos
optimizeCondStmt (Cond pos cond stmt) = Cond pos cond (optimizeCondStmt stmt)
optimizeCondStmt (CondElse pos (ELitTrue _) stmt _) = BStmt pos (Block pos [optimizeCondStmt stmt])
optimizeCondStmt (CondElse pos (ELitFalse _) _ stmt) = BStmt pos (Block pos [optimizeCondStmt stmt])
optimizeCondStmt (CondElse pos cond stmt1 stmt2) = CondElse pos cond (optimizeCondStmt stmt1) (optimizeCondStmt stmt2)
optimizeCondStmt (While pos (ELitFalse _) stmt) = Empty pos
optimizeCondStmt (While pos cond stmt) = While pos cond (optimizeCondStmt stmt)
optimizeCondStmt (For pos t i e stmt) = For pos t i e (optimizeCondStmt stmt)
optimizeCondStmt stmt = stmt

optimizeExprInStmts :: [Stmt a] -> [Stmt a]
optimizeExprInStmts stmts = map optimizeExprInStmt stmts

optimizeExprInStmt :: Stmt a -> Stmt a
optimizeExprInStmt (BStmt pos1 (Block pos2 stmts)) = (BStmt pos1 (Block pos2 (optimizeExprInStmts stmts)))
optimizeExprInStmt (Decl pos t items) = Decl pos t (map optimizeExprInItem items) where
  optimizeExprInItem (NoInit pos i) = (NoInit pos i)
  optimizeExprInItem (Init pos ident expr) = Init pos ident (optimizeExpr expr)
optimizeExprInStmt (Ass pos e1 e2) = Ass pos e1 (optimizeExpr e2)
optimizeExprInStmt (Ret pos e) = Ret pos (optimizeExpr e)
optimizeExprInStmt (Cond pos condE stmt) = Cond pos (optimizeExpr condE) (optimizeExprInStmt stmt)
optimizeExprInStmt (CondElse pos condE stmt1 stmt2) = CondElse pos (optimizeExpr condE) (optimizeExprInStmt stmt1) (optimizeExprInStmt stmt2)
optimizeExprInStmt (While pos e stmt) = While pos (optimizeExpr e) (optimizeExprInStmt stmt)
optimizeExprInStmt (For pos t i e stmt) = For pos t i e (optimizeExprInStmt stmt)
optimizeExprInStmt (SExp pos e) = SExp pos (optimizeExpr e)
optimizeExprInStmt stmt = stmt

optimizeExpr :: Expr a -> Expr a
optimizeExpr (ENewArray pos t index) = (ENewArray pos t (optimizeExpr index))
optimizeExpr (EApp pos fName exprs) = (EApp pos fName (map optimizeExpr exprs))
optimizeExpr (EArrayItem pos varName index) = (EArrayItem pos varName (optimizeExpr index))
optimizeExpr (Neg pos expr) = case oExpr of
    ELitTrue _ -> ELitFalse pos
    ELitFalse _ -> ELitTrue pos
    _ -> (Neg pos oExpr)
  where oExpr = optimizeExpr expr
optimizeExpr (Not pos expr) = (Not pos (optimizeExpr expr))
optimizeExpr (EMul pos e1 op e2) =
  case (oe1, oe2) of
    (ELitInt _ x, ELitInt _ y) -> (ELitInt pos (performMulOp op x y))
    _ -> (EMul pos oe1 op oe2)
  where
    oe1 = optimizeExpr e1
    oe2 = optimizeExpr e2
optimizeExpr (EAdd pos e1 op e2) =
  case (oe1, oe2) of
    (ELitInt _ x, ELitInt _ y) -> (ELitInt pos (performAddOp op x y))
    _ -> (EAdd pos oe1 op oe2)
  where
    oe1 = optimizeExpr e1
    oe2 = optimizeExpr e2
optimizeExpr (ERel pos e1 op e2) =
  case (oe1, oe2) of
    (ELitInt _ x, ELitInt _ y) -> performRelOp op x y pos
    _ -> (ERel pos oe1 op oe2)
  where
    oe1 = optimizeExpr e1
    oe2 = optimizeExpr e2
optimizeExpr (EAnd pos e1 e2) =
  case (oe1, oe2) of
    (ELitTrue _, ELitFalse _) -> ELitFalse pos
    (ELitFalse _, ELitFalse _) -> ELitFalse pos
    (ELitFalse _, ELitTrue _) -> ELitFalse pos
    (ELitTrue _, ELitTrue _) -> ELitTrue pos
    _ -> (EAnd pos oe1 oe2)
    where
      oe1 = optimizeExpr e1
      oe2 = optimizeExpr e2
optimizeExpr (EOr pos e1 e2) =
  case (oe1, oe2) of
    (ELitTrue _, ELitFalse _) -> ELitTrue pos
    (ELitFalse _, ELitFalse _) -> ELitFalse pos
    (ELitFalse _, ELitTrue _) -> ELitTrue pos
    (ELitTrue _, ELitTrue _) -> ELitTrue pos
    _ -> (EOr pos oe1 oe2)
    where
      oe1 = optimizeExpr e1
      oe2 = optimizeExpr e2
optimizeExpr e = e

performMulOp :: MulOp a -> Integer -> Integer -> Integer
performMulOp (Times _) x y = x*y
performMulOp (Div _) x y = div x y
performMulOp (Mod _) x y = mod x y

performAddOp :: AddOp a -> Integer -> Integer -> Integer
performAddOp (Plus _) x y = x + y
performAddOp (Minus _) x y = x - y

performRelOp :: RelOp a -> Integer -> Integer -> a -> Expr a
performRelOp (LTH _) x y pos =
  if x < y then ELitTrue pos
    else ELitFalse pos
performRelOp (LE _) x y pos =
  if x <= y then ELitTrue pos
    else ELitFalse pos
performRelOp (GTH _) x y pos =
  if x > y then ELitTrue pos
    else ELitFalse pos
performRelOp (GE _) x y pos =
  if x >= y then ELitTrue pos
    else ELitFalse pos
performRelOp (EQU _) x y pos =
  if x == y then ELitTrue pos
    else ELitFalse pos
performRelOp (NE _) x y pos =
  if x /= y then ELitTrue pos
    else ELitFalse pos

checkFunctionDef :: TopDef PositionInfo -> Frontend ()
checkFunctionDef (FnDef _ _ name args (Block _ stmts)) = do
  let argNames = map getArgName args
  areElementsUnique ("In function " ++ (getIdentString name) ++ " argument ") argNames
  env <- ask
  let env' = argNames ++ env
  local (const env') (checkStmts env' stmts)
  return ()

checkStmts :: [String] -> [Stmt PositionInfo] -> Frontend [String]
checkStmts used stmts = foldlM checkStmt used stmts

checkStmt :: [String] -> Stmt PositionInfo -> Frontend [String]
checkStmt used (BStmt _ (Block _ blockStmts)) = do
  env <- ask
  checkStmts env blockStmts
  return used
checkStmt used (Decl _ _ items) = do
  foldlM checkItem used items
  where
    checkItem :: [String] -> Item PositionInfo -> Frontend [String]
    checkItem used (NoInit pos name) = checkItemName used name pos
    checkItem used (Init pos name _) = checkItemName used name pos
    checkItemName used name pos = do
      if elem (getIdentString name) used then
        writeVariableDefined (getIdentString name) pos
      else
        return ((getIdentString name):used)
    writeVariableDefined :: String -> PositionInfo -> Frontend [String]
    writeVariableDefined name (Just (line, pos)) = do
      throwError $ "Variable " ++ name ++ " defined at line " ++ (show line) ++ " pos " ++ (show pos) ++ " has already been defined"
    writeVariableDefined name Nothing = do
      throwError $ "Variable " ++ name ++ " has already been defined"
checkStmt used (Cond _ _ stmt) = do
  env <- ask
  checkStmt env stmt
  return used
checkStmt used (CondElse _ _ stmt1 stmt2) = do
  env <- ask
  checkStmt env stmt1
  checkStmt env stmt2
  return used
checkStmt used (While _ _ stmt) = do
  env <- ask
  checkStmt env stmt
  return used
checkStmt used (For _ _ _ _ stmt) = do
  env <- ask
  checkStmt env stmt
  return used
checkStmt used _ = do
  return used

-- check wether class variables are unique
uniqueClassVars :: ClassDef PositionInfo -> Frontend ()
uniqueClassVars (ClDef _ name elements) = do
  let elementNames = map getClassElementName elements
  areElementsUnique ("In class " ++ (getIdentString name) ++ " element ") elementNames

-- if elements in list are not unique throw error
areElementsUnique :: (Show a, Eq a) => String -> [a] -> Frontend ()
areElementsUnique x y = f [] x y where
  f :: (Show a, Eq a) => [a] -> String -> [a] -> Frontend ()
  f _ _ [] = return ()
  f seenElements errMsg (x:xs) = do
    if elem x seenElements then
      throwError $ errMsg ++ (show x) ++ " redefined\n"
    else
      f (x:seenElements) errMsg xs
