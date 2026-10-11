module TypeCheck (typeCheck, runTypeCheck, getClassesMapping) where

import AbsLatte

import Data.Maybe
import Data.List (sort)
import Data.Foldable (foldlM)

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Map.Lazy as Map

import Misc

type TypeMapping = Map.Map String T
type Store = (TypeMapping, Map.Map String Struct) -- store for functions and structures
type TypeCheck a = ErrorT String (StateT Store Identity) a

runTypeCheck :: TypeCheck a -> (Either String a, Store)
runTypeCheck tc = runIdentity (runStateT (runErrorT tc) (Map.empty, Map.empty))

typeCheck :: Program PositionInfo -> TypeCheck ()
typeCheck (Program _ classes functions) = do
  -- get classes and functions mapping
  let classesMapping = getClassesMapping classes
  let functionsMapping = Map.union (getFunctionsMapping functions)
                                    (Map.fromList [("printInt", Fun V [Integer]),
                                                   ("printString", Fun V [String]),
                                                   ("error", Fun V []),
                                                   ("readInt", Fun Integer []),
                                                   ("readString", Fun String [])])
  -- TODO: check if all classes exist
  -- check that main exists and has correct signature
  case (Map.lookup "main" functionsMapping) of
    Nothing -> throwError "Function main() not found"
    Just (Fun Integer []) -> do
      put (functionsMapping, classesMapping)
      mapM_ typeCheckFunction functions
      return ()
    _ -> throwError "Function main() has wrong signature, int main() expected"
  return ()

typeCheckFunction :: TopDef PositionInfo -> TypeCheck ()
typeCheckFunction (FnDef _ t _ args (Block _ stmts)) = do
  typeCheckStmts ((argsMapping args), (typeConvert t)) stmts
  return () where
  argsMapping :: [Arg PositionInfo] -> TypeMapping
  argsMapping args = foldl argMapping Map.empty args
  argMapping :: TypeMapping -> Arg a -> TypeMapping
  argMapping tm (Arg _ t name) = Map.insert (getIdentString name) (typeConvert t) tm

-- first argument is pair: type mapping and expected return type
typeCheckStmts :: (TypeMapping, T) -> [Stmt PositionInfo] -> TypeCheck (TypeMapping, T)
typeCheckStmts m stmts = foldlM typeCheckStmt m stmts

typeCheckStmt :: (TypeMapping, T) -> Stmt PositionInfo -> TypeCheck (TypeMapping, T)
typeCheckStmt m (Empty _) = return m
typeCheckStmt m (BStmt _ (Block _ stmts)) = do
  typeCheckStmts m stmts
  return m
typeCheckStmt (mapping, retVal) (Decl _ t items) = do
  newMapping <- foldT mapping (typeConvert t) items
  return (newMapping, retVal) where
  foldT :: TypeMapping -> T -> [Item PositionInfo] -> TypeCheck TypeMapping
  foldT tm t [] = return tm
  foldT tm t (item:items) = do
    tmp <- addElem tm t item
    foldT tmp t items
  addElem :: TypeMapping -> T -> Item PositionInfo -> TypeCheck TypeMapping
  addElem tm t (NoInit _ ident) = return (Map.insert (getIdentString ident) t tm)
  addElem tm t (Init pos ident expr) = do
    rhsType <- typeCheckExpr tm expr
    if rhsType /= t then do
      writeError "Right hand side of variable declaration is in different type" pos
      return tm
    else
      return (Map.insert (getIdentString ident) t tm)
typeCheckStmt _ (Ass pos (EApp _ _ _) _) = do
  writeError "Function result is not assignable" pos
  throwError ""
typeCheckStmt (tm, retVal) (Ass pos assignItem expr) = do
  itemType <- typeCheckExpr tm assignItem
  exprType <- typeCheckExpr tm expr
  if itemType /= exprType then do
    writeError "Assignment type mismatch" pos
    throwError ""
  else
    return (tm, retVal)
typeCheckStmt _ (Incr pos (EApp _ _ _)) = do
  writeError "Function result is not assignable" pos
  throwError ""
typeCheckStmt (tm, retVal) (Incr pos item) = do
  itemType <- typeCheckExpr tm item
  if itemType /= Integer then do
    writeError "Incremented value must be an int" pos
    throwError ""
  else
    return (tm, retVal)
typeCheckStmt _ (Decr pos (EApp _ _ _)) = do
  writeError "Function result is not assignable" pos
  throwError ""
typeCheckStmt (tm, retVal) (Decr pos item) = do
  itemType <- typeCheckExpr tm item
  if itemType /= Integer then do
    writeError "Decremented value must be an int" pos
    throwError ""
  else
    return (tm, retVal)
typeCheckStmt (tm, retVal) (Ret pos expr) = do
  t <- typeCheckExpr tm expr
  if t /= retVal then do
    writeError "Return value type doesn't match function" pos
    throwError ""
  else
    return (tm, retVal)
typeCheckStmt (tm, retVal) (VRet pos) = do
  if V /= retVal then do
    writeError "Return value type doesn't match function" pos
    throwError ""
  else
    return (tm, retVal)
typeCheckStmt (tm, retVal) (Cond pos expr stmt) = do
  typeCheckCond tm expr pos
  typeCheckStmt (tm, retVal) stmt
  return (tm, retVal)
typeCheckStmt (tm, retVal) (CondElse pos expr stmt1 stmt2) = do
  typeCheckCond tm expr pos
  typeCheckStmt (tm, retVal) stmt1
  typeCheckStmt (tm, retVal) stmt2
  return (tm, retVal)
typeCheckStmt (tm, retVal) (While pos expr stmt) = do
  typeCheckCond tm expr pos
  typeCheckStmt (tm, retVal) stmt
  return (tm, retVal)
typeCheckStmt (tm, retVal) (For pos t name item stmt) = do
  t1 <- typeCheckExpr tm item
  case t1 of
    A t2 -> do
      if t2 /= (typeConvert t) then do
        writeError "Wrong type in for initialization" pos
        throwError ""
      else do
        typeCheckStmt (Map.insert (getIdentString name) (typeConvert t) tm, retVal) stmt
        return (tm, retVal)
    _ -> do
      writeError "For must refer to an array" pos
      throwError ""
typeCheckStmt (tm, retVal) (SExp _ expr) = do
  typeCheckExpr tm expr
  return (tm, retVal)

typeCheckCond :: TypeMapping -> Expr PositionInfo -> PositionInfo -> TypeCheck ()
typeCheckCond tm e pos = do
  t <- typeCheckExpr tm e
  if t /= Boolean then do
    writeError "Condition must be a boolean" pos
  else
    return ()

typeCheckExpr :: TypeMapping -> Expr PositionInfo -> TypeCheck T
typeCheckExpr tm (ENull pos ident) = do
  (_, classesMapping) <- get
  if Map.member (getIdentString ident) classesMapping then
    return $ Class ident
  else do
    writeError ((getIdentString ident) ++ " does not denote a class name") pos
    throwError $ ""
typeCheckExpr tm (EVar pos ident) = do
  let item = Map.lookup (getIdentString ident) tm
  case item of
    Nothing -> do
      writeError ("Variable " ++ (getIdentString ident) ++ " does not exist ") pos
      throwError $ ""
    Just x -> return x
typeCheckExpr tm (ELitInt _ _) = return Integer
typeCheckExpr tm (ELitTrue _) = return Boolean
typeCheckExpr tm (ELitFalse _) = return Boolean
typeCheckExpr tm (EApp pos fName exprs) = do
  args <- mapMM tm exprs []
  (functionsMapping, _) <- get
  let f = Map.lookup (getIdentString fName) functionsMapping
  case f of
    Nothing -> do
      writeError ("Function " ++ (getIdentString fName) ++ " does not exist") pos
      throwError $ ""
    Just t -> do
      case t of
        Fun retType expectedArgs -> do
          if (sort expectedArgs) == (sort args) then
            return retType
          else do
            writeError ("Argument type mismatch ") pos
            throwError $ ""
        _ -> do
          writeError ((getIdentString fName) ++ " is not a function") pos
          throwError $ ""
typeCheckExpr tm (EString _ _) = return String
typeCheckExpr tm (Neg pos expr) = do
  exprType <- typeCheckExpr tm expr
  case exprType of
    Integer -> return Integer
    _ -> do
      writeError "Negetion of something different than integer" pos
      throwError $ ""
typeCheckExpr tm (Not pos expr) = do
  exprType <- typeCheckExpr tm expr
  case exprType of
    Boolean -> return Boolean
    _ -> do
      writeError "Not of something different than boolean" pos
      throwError $ ""
typeCheckExpr tm (EMul pos e1 _ e2) = typeCheckExprBin tm e1 e2 pos Integer Integer
typeCheckExpr tm (EAdd pos e1 (Plus _) e2) = do
  t1 <- typeCheckExpr tm e1
  t2 <- typeCheckExpr tm e2
  if or [t1/=t2, not (elem t1 [String, Integer])] then do
    writeError "Wrong type of arguments in addition" pos
    throwError $ ""
  else return t1
typeCheckExpr tm (EAdd pos e1 _ e2) = typeCheckExprBin tm e1 e2 pos Integer Integer
typeCheckExpr tm (EAnd pos e1 e2) = typeCheckExprBin tm e1 e2 pos Boolean Boolean
typeCheckExpr tm (EOr pos e1 e2) = typeCheckExprBin tm e1 e2 pos Boolean Boolean
typeCheckExpr tm (ERel pos e1 (LTH _) e2) = typeCheckExprBin tm e1 e2 pos Integer Boolean
typeCheckExpr tm (ERel pos e1 (LE _) e2) = typeCheckExprBin tm e1 e2 pos Integer Boolean
typeCheckExpr tm (ERel pos e1 (GTH _) e2) = typeCheckExprBin tm e1 e2 pos Integer Boolean
typeCheckExpr tm (ERel pos e1 (GE _) e2) = typeCheckExprBin tm e1 e2 pos Integer Boolean
typeCheckExpr tm (ERel pos e1 _ e2) = do
  t1 <- typeCheckExpr tm e1
  t2 <- typeCheckExpr tm e2
  if (t1 /= t2) then do
    writeError "Both sides of relOp should have the same type" pos
    throwError $ ""
  else
    return Boolean
typeCheckExpr tm (ENew pos t) = do
  case t of
    ClassT _ name -> do
      (_, classesMapping) <- get
      case Map.lookup (getIdentString name) classesMapping of
        Just x -> return $ Class name
        _ -> do
          writeError "Class with this name doesn't exist" pos
          throwError $ ""
    _ -> do
      writeError "New can be applied only to class type" pos
      throwError $ ""
typeCheckExpr tm (ENewArray pos t expr) = do
  indexType <- typeCheckExpr tm expr
  case indexType of
    Integer -> return $ A (typeConvert t)
    _ -> do
      writeError "Array index must be an integer" pos
      throwError $ ""
typeCheckExpr tm (EField pos struct itemName) = do
  structType <- typeCheckExpr tm struct
  case structType of
    A _ -> do
      if (getIdentString itemName) == "length" then
        return Integer
      else do
        writeError "Only length can be applied to an array" pos
        throwError ""
    Class className -> do
      (_, classesMapping) <- get
      let classNameStr = getIdentString className
      let classMapping = Map.lookup classNameStr classesMapping
      case classMapping of
        Nothing -> do
          writeError "Class doesn't exist" pos
          throwError ""
        Just mapping -> do
          case (Map.lookup (getIdentString itemName) mapping) of
            Nothing -> do
              writeError "Class field doesn't exist" pos
              throwError ""
            Just t -> do
              return t
    _ -> do
      writeError "Field reference can be done only in structs and strings" pos
      throwError ""
typeCheckExpr tm (EArrayItem pos item index) = do
  itemType <- typeCheckExpr tm item
  indexType <- typeCheckExpr tm index
  case itemType of
    A t -> do
      case indexType of
        Integer -> return t
        _ -> do
          writeError "Array index must by an integer " pos
          throwError $ ""
    _ -> do
      writeError "Tried to access non array by index " pos
      throwError $ ""

typeCheckExprBin :: TypeMapping -> Expr PositionInfo -> Expr PositionInfo -> PositionInfo -> T -> T -> TypeCheck T
typeCheckExprBin tm expr1 expr2 pos expectedType resType = do
  t1 <- typeCheckExpr tm expr1
  t2 <- typeCheckExpr tm expr2
  if (or [(t2 /= expectedType), (t1 /= expectedType)]) then do
    writeError "Wrong type of arguments in binary expression" pos
    throwError $ ""
  else
    return resType

mapMM :: TypeMapping -> [Expr PositionInfo] -> [T] -> TypeCheck [T]
mapMM  _ [] ts = return ts
mapMM tm (expr:exprs) types = do
  newType <- typeCheckExpr tm expr
  mapMM tm exprs (newType:types)

writeError :: String -> PositionInfo -> TypeCheck ()
writeError errMsg pos = do
  case pos of
    Just (line, position) -> throwError $ errMsg ++ " in line " ++ (show line) ++ " pos " ++ (show position)
    Nothing -> throwError $ errMsg

getFunctionsMapping :: [TopDef a] -> Map.Map String T
getFunctionsMapping functions = foldl insertFunction Map.empty functions where
  insertFunction :: Map.Map String T -> TopDef a -> Map.Map String T
  insertFunction m (FnDef _ retType name args _) = Map.insert (getIdentString name) (Fun (typeConvert retType) (map getType args)) m
  getType :: Arg a -> T
  getType (Arg _ t _) = typeConvert t


getClassesMapping :: [ClassDef a] -> Map.Map String Struct
getClassesMapping classes = foldl insertClass Map.empty classes where
  insertClass :: Map.Map String Struct -> ClassDef a -> Map.Map String Struct
  insertClass m (ClDef _ name elements) = Map.insert (getIdentString name) (getClassMapping elements) m

getClassMapping :: [ClassEl a] -> Struct
getClassMapping elements = collectElements (Map.empty) elements where
  collectElements :: Map.Map String T -> [ClassEl a] -> Map.Map String T
  collectElements mapping []  = mapping
  collectElements mapping ((ClElem _ t name):els) = collectElements (Map.insert (getIdentString name) (typeConvert t) mapping) els


typeConvert :: Type a -> T
typeConvert (Int _) = Integer
typeConvert (Str _) = String
typeConvert (Bool _) = Boolean
typeConvert (Void _) = V
typeConvert (ClassT _ name) = Class name
typeConvert (Array _ t) = A (typeConvert t)
