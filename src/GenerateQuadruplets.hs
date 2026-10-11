module GenerateQuadruplets (runGenerateQuadruplets, generateAll) where

import AbsLatte

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Identity

import qualified Data.Map.Lazy as Map

import Misc
import Quadruplets

type Store = (Integer, Integer, SimpleBlock, [SimpleBlock], StructMap, FunctionType)
-- next fresh label, next var, current block, list of blocks, functiont type mapping
type VariableMapping = Map.Map String Var
type FunctionType = Map.Map String QType
type Env = VariableMapping -- mapping variable->label
type GenerateQuadruplets a = ReaderT Env (ErrorT String (StateT Store Identity)) a

runGenerateQuadruplets :: GenerateQuadruplets a -> StructMap -> (Either String a, Store)
runGenerateQuadruplets quad sm =
  runIdentity (runStateT (runErrorT (runReaderT quad Map.empty)) (1, 0, (SimpleBlock "L0" [] []), [], sm, Map.empty))

generateAll :: [TopDef PositionInfo] -> GenerateQuadruplets [[SimpleBlock]]
generateAll topDefs = do
  (nextL, nextV, block, blocks, structMap, _) <- get
  put (nextL, nextV, block, [], structMap, collectFunctionTypeMapping topDefs)
  mapM generateQuadruplets topDefs

collectFunctionTypeMapping :: [TopDef PositionInfo] -> FunctionType
collectFunctionTypeMapping fs =
  Map.insert "printInt" QVal (
  Map.insert "printString" QVal (
  Map.insert "error" QVal (
  Map.insert "readInt" QVal (
  Map.insert "readString" QString m))))
  where m = Map.fromList (map functionType fs)

functionType :: TopDef PositionInfo -> (String, QType)
functionType (FnDef _ t ident _ _) = (getIdentString ident, getType t)

generateQuadruplets :: TopDef PositionInfo -> GenerateQuadruplets [SimpleBlock]
generateQuadruplets (FnDef _ _ _ args (Block _ stmts)) = do
  local (const (functionArgsMapping args Map.empty)) (generateStmts stmts)
  (nextL, nextV, block, blocks, structMap, fType) <- get
  put (nextL, nextV, block, [], structMap, fType)
  return $ reverse blocks

functionArgsMapping :: [Arg a] -> VariableMapping -> VariableMapping
functionArgsMapping ((Arg _ t name):args) mapping =
  functionArgsMapping args (Map.insert (getIdentString name) (Var (getType t) (getIdentString name)) mapping)
functionArgsMapping [] mapping = mapping

generateStmts :: [Stmt a] -> GenerateQuadruplets ()
generateStmts (stmt:stmts) = do
  env <- generateStmt stmt
  local (const env) (generateStmts stmts)
generateStmts [] = do
  label <- freshLabel
  (nextL, nextV, block, blocks, structMap, fType) <- get
  put (nextL, nextV, (SimpleBlock label [] []), (block:blocks), structMap, fType)
  return ()

generateStmt :: Stmt a -> GenerateQuadruplets Env
generateStmt (Empty _) = ask
generateStmt (BStmt _ (Block _ stmts)) = do
  label <- freshLabel
  (nextL, nextV, block, blocks, structMap, fType) <- get
  put (nextL, nextV, (SimpleBlock label [] []), (block:blocks), structMap, fType)
  generateStmts stmts
  ask
generateStmt (Decl _ t items) = do
  env <- ask
  generateItems t items env
generateStmt (Ass _ lhs expr) = do
  value <- generateExpr expr
  generateLValue lhs value
  ask
generateStmt (Incr _ expr) = do
  changeLValue expr 1 Null
  ask
generateStmt (Decr _ expr) = do
  changeLValue expr (-1) Null
  ask
generateStmt (Ret _ expr) = do
  v <- generateExpr expr
  addToBlock (QReturn v)
  ask
generateStmt (VRet _) = do
  addToBlock (VReturn)
  ask
generateStmt (Cond _ cond stmt) = do
  c <- generateExpr cond
  lThen <- freshLabel
  lAfter <- freshLabel
  addToBlock (CFJump c lAfter)
  flushBlock lThen
  generateStmt stmt
  flushBlock lAfter
  ask
generateStmt (CondElse _ cond stmtThen stmtElse) = do
  c <- generateExpr cond
  lThen <- freshLabel
  lElse <- freshLabel
  lAfter <- freshLabel
  addToBlock (CFJump c lElse)
  flushBlock lThen
  generateStmt stmtThen
  addToBlock (Jump lAfter)
  flushBlock lElse
  generateStmt stmtElse
  flushBlock lAfter
  ask
generateStmt (While _ cond stmt) = do
  lBody <- freshLabel
  lCond <- freshLabel
  addToBlock (Jump lCond)
  flushBlock lBody
  generateStmt stmt
  flushBlock lCond
  c <- generateExpr cond
  addToBlock (CTJump c lBody)
  ask
generateStmt (For _ t ident array stmt) = do
  lBody <- freshLabel
  lCond <- freshLabel
  iTemp <- freshTemp
  aLength <- freshTemp
  a <- generateExpr array
  let i = (Var QVal iTemp)
  let aLen = (Var QVal aLength)
  addLiveVar i
  addLiveVar aLen
  addToBlock (ConstInt i 0)
  addToBlock (ArrayLen aLen a)
  addToBlock (Jump lCond)
  flushBlock lBody
  elTemp <- freshTemp
  let el = (Var (arrayType a) elTemp)
  addToBlock (ArrayElem el a i)
  addLiveVar el
  env <- ask
  let env' = Map.insert (getIdentString ident) el env
  local (const env') (generateStmt stmt)
  addToBlock (Increment i)
  addLiveVar i
  flushBlock lCond
  addToBlock (LTJump i aLen lBody)
  addLiveVar i
  next <- freshLabel
  flushBlock next
  ask
generateStmt (SExp _ expr) = do
  generateExpr expr
  ask

generateLValue :: Expr a -> Var -> GenerateQuadruplets ()
generateLValue expr val = changeLValue expr 0 val

-- 0 - change to var
-- 1 - Increment
-- -1 - decrement
changeLValue :: Expr a -> Integer -> Var -> GenerateQuadruplets ()
changeLValue (EVar _ ident) increment value = do
  varMap <- ask
  case (Map.lookup (getIdentString ident) varMap) of
    Just v -> do
      if increment == 0 then
        addToBlock (Assign v value)
      else
        if increment == 1 then
          addToBlock (Increment v)
        else
          addToBlock (Decrement v)
      addLiveVar v
      return ()
changeLValue (EField _ expr ident) increment val = do
  struct <- generateExpr expr
  if increment == 0 then
    addToBlock (StoreStruct struct (getIdentString ident) val)
  else
    if increment == 1 then
      addToBlock (IncrementField struct (getIdentString ident))
    else
      addToBlock (DecrementField struct (getIdentString ident))
  return ()
changeLValue (EArrayItem _ e1 e2) increment val = do
  array <- generateExpr e1
  index <- generateExpr e2
  if increment == 0 then
    addToBlock (StoreArray array index val)
  else
    if increment == 1 then
      addToBlock (IncrementArr array index)
    else
      addToBlock (DecrementArr array index)
  return ()

generateItems :: Type a -> [Item a] -> Env -> GenerateQuadruplets Env
generateItems _ [] env = return env
generateItems t (i:is) env = do
  env' <- generateItem t i env
  generateItems t is env'

generateItem :: Type a -> Item a -> Env -> GenerateQuadruplets Env
generateItem t (NoInit _ ident) env = do
  label <- freshTemp
  let v = (Var (getType t) label)
  let env' = Map.insert (getIdentString ident) v env
  addLiveVar v
  case t of
    (Str _) -> do
      addToBlock (ConstString v "")
    _ -> do
      addToBlock (ConstInt v 0)
  return env'
generateItem t (Init _ ident expr) env = do
  temp <- freshTemp
  let v = (Var (getType t) temp)
  let env' = Map.insert (getIdentString ident) v env
  e <- generateExpr expr
  addToBlock (Assign v e)
  addLiveVar v
  return env'

generateExpr :: Expr a -> GenerateQuadruplets Var
generateExpr (ENew _ t) = do
  label <- freshTemp
  let v = (Var (getType t) label)
  structSize <- getStructSize t
  addToBlock (Malloc v (IConst structSize))
  return v
generateExpr (ENewArray _ t expr) = do
  case (getType t) of
    QVal -> generateNewArray t intSize expr
    _ -> generateNewArray t ptrSize expr
generateExpr (EVar _ ident) = do
  env <- ask
  case Map.lookup (getIdentString ident) env of
    Just x -> return x
    Nothing -> throwError $ "Couldn't find ident of variable"
generateExpr (EApp _ fName params) = do
  ps <- mapM generateExpr params
  case (getIdentString fName) of
    "error" -> do
      addToBlock Error
      retEmpty
    "printString" -> do
      let (strName:_) = ps
      addToBlock (PrintString strName)
      retEmpty
    "printInt" -> do
      let (intName:_) = ps
      addToBlock (PrintInt intName)
      retEmpty
    "readInt" -> do
      temp <- freshTemp
      let resVar = (Var QVal temp)
      addToBlock (ReadInt resVar)
      return resVar
    "readString" -> do
      temp <- freshTemp
      let resVar = (Var QString temp)
      addToBlock (ReadStr resVar)
      return resVar
    _ -> do
      temp <- freshTemp
      (_, _, _, _, _, fTypeMap) <- get
      let fType = maybe QVal id (Map.lookup (getIdentString fName) fTypeMap)
      let v = (Var fType temp)
      addToBlock (Call (getIdentString fName) v ps)
      return v
generateExpr (EField _ struct field) = do
  temp <- freshTemp
  p <- generateExpr struct
  case p of
    (Var (QClass name) t) -> do
      let fieldStr = getIdentString field
      fieldType <- getStructElType name fieldStr
      let v = (Var fieldType temp)
      let q = (ClassElem v (Var (QClass name) t) fieldStr)
      addToBlock q
      return v
    (Var (QArrayRef t) name) -> do
      let v = (Var QVal temp)
      addToBlock (ArrayLen v (Var (QArrayRef t) name))
      return v
generateExpr (EArrayItem _ array index) = do
  (Var (QArrayRef t) aName) <- generateExpr array
  indexV <- generateExpr index
  temp <- freshTemp
  let v = (Var t temp)
  addToBlock (ArrayElem v (Var (QArrayRef t) aName) indexV)
  return v
generateExpr (ENull _ _) = return Null
generateExpr (ELitInt _ x) = return (IConst x)
generateExpr (ELitTrue _) = return QTrue
generateExpr (ELitFalse _) = return QFalse
generateExpr (EString _ str) = do
  temp <- freshTemp
  addToBlock (ConstString (Var QString temp) str)
  return $ (Var QString temp)
generateExpr (Neg _ expr) = do
  e <- generateExpr expr
  case e of
    (Var t l) -> do
      temp <- freshTemp
      let v = (Var t temp)
      addToBlock (Negate v e)
      return v
    (IConst x) -> do
      return (IConst (-1*x))
generateExpr (Not _ expr) = do
  e <- generateExpr expr
  case e of
    (Var t l) -> do
      lTrue <- freshLabel
      lFalse <- freshLabel
      lAfter <- freshLabel
      temp <- freshTemp
      let v = (Var QVal temp)
      addToBlock (CTJump (Var t l) lTrue)
      flushBlock lFalse
      addToBlock (ConstInt v 1)
      addToBlock (Jump lAfter)
      addLiveVar v
      flushBlock lTrue
      addToBlock (ConstInt v 0)
      addLiveVar v
      flushBlock lAfter
      return v
    QTrue -> return QFalse
    QFalse -> return QTrue
generateExpr (EMul _ e1 op e2) = do
  e1v <- generateExpr e1
  e2v <- generateExpr e2
  temp <- freshTemp
  let v = (Var QVal temp)
  addToBlock (BinOp v e1v (getBinOpMul op) e2v)
  return v
generateExpr (EAdd _ e1 op e2) = do
  e1v <- generateExpr e1
  e2v <- generateExpr e2
  temp <- freshTemp
  case e1v of
    (Var QString _) -> do -- string concat
      let v = (Var QString temp)
      addToBlock (Concat v e1v e2v)
      return v
    _ -> do
      let v = (Var QVal temp)
      addToBlock (BinOp v e1v (getBinOpAdd op) e2v)
      return v
generateExpr (EAnd _ e1 e2) = do
  temp <- freshTemp
  lCon2 <- freshLabel
  lTrue <- freshLabel
  lFalse <- freshLabel
  lThen <- freshLabel
  e1v <- generateExpr e1
  addToBlock (CFJump e1v lFalse)
  flushBlock lCon2
  e2v <- generateExpr e2
  addToBlock (CTJump e2v lTrue)
  flushBlock lFalse
  -- block false
  let v = (Var QVal temp)
  addLiveVar v
  addToBlock (ConstInt v 0)
  addToBlock (Jump lThen)
  flushBlock lTrue
  -- block true
  addLiveVar v
  addToBlock (ConstInt v 1)
  flushBlock lThen
  return v
generateExpr (EOr _ e1 e2) = do
  temp <- freshTemp
  lCon2 <- freshLabel
  lTrue <- freshLabel
  lFalse <- freshLabel
  lThen <- freshLabel
  e1v <- generateExpr e1
  addToBlock (CTJump e1v lTrue)
  flushBlock lCon2
  e2v <- generateExpr e2
  addToBlock (CTJump e2v lTrue)
  flushBlock lFalse
  -- block false
  let v = (Var QVal temp)
  addLiveVar v
  addToBlock (ConstInt v 0)
  addToBlock (Jump lThen)
  flushBlock lTrue
  -- block true
  addLiveVar v
  addToBlock (ConstInt v 1)
  flushBlock lThen
  return v
generateExpr (ERel _ e1 op e2) = do
  e1v <- generateExpr e1
  e2v <- generateExpr e2
  temp <- freshTemp
  let v = (Var QVal temp)
  lTrue <- freshLabel
  lFalse <- freshLabel
  lAfter <- freshLabel
  generateJump e1v e2v lTrue op
  let v = (Var QVal temp)
  flushBlock lFalse
  addToBlock (ConstInt v 0)
  addToBlock (Jump lAfter)
  addLiveVar v
  flushBlock lTrue
  addToBlock (ConstInt v 1)
  addLiveVar v
  flushBlock lAfter
  return v

retEmpty :: GenerateQuadruplets Var
retEmpty = do
  temp <- freshTemp
  return (Var QVal temp)

-- LTH a | LE a | GTH a | GE a | EQU a | NE a
generateJump :: Var -> Var -> String -> RelOp a -> GenerateQuadruplets ()
generateJump v1 v2 targetBlock (LTH _) = addToBlock (LTJump v1 v2 targetBlock)
generateJump v1 v2 targetBlock (LE _) = addToBlock (LEJump v1 v2 targetBlock)
generateJump v1 v2 targetBlock (GTH _) = addToBlock (GTJump v1 v2  targetBlock)
generateJump v1 v2 targetBlock (GE _) = addToBlock (GEJump v1 v2 targetBlock)
generateJump v1 v2 targetBlock (EQU _) = addToBlock (EQJump v1 v2 targetBlock)
generateJump v1 v2 targetBlock (NE _) = addToBlock (NEQJump v1 v2 targetBlock)

getBinOpMul :: MulOp a -> BinOp
getBinOpMul (Times _) = QTimes
getBinOpMul (Div _) = QDiv
getBinOpMul (Mod _) = QMod

getBinOpAdd :: AddOp a -> BinOp
getBinOpAdd (Plus _) = QPlus
getBinOpAdd (Minus _) = QMinus

generateNewArray :: Type a -> Integer -> (Expr a) -> GenerateQuadruplets Var
generateNewArray t size expr = do
  label <- freshTemp
  let v = (Var (QArrayRef (getType t)) label)
  case expr of
    (ELitInt _ int) -> do
      addToBlock (ArrayMalloc v (IConst (size*int+intSize)) (IConst int))
      return v
    _ -> do
      label2 <- freshTemp
      let v2 = (Var QVal label2)
      arraySize <- generateExpr expr
      addToBlock (BinOp v2 (IConst size) QTimes arraySize)
      label3 <- freshTemp
      let v3 = (Var QVal label3)
      addToBlock (BinOp v3 (IConst intSize) QPlus v2)
      addToBlock (ArrayMalloc v v3 arraySize)
      return v

getStructSize :: Type a -> GenerateQuadruplets Integer
getStructSize t = do
  case t of
    (ClassT _ ident) -> do
      (_, _, _, _, sm, _) <- get
      case Map.lookup (getIdentString ident) sm of
        Just m -> return $ getSizes $ Map.toList m

getSizes :: [(String, T)] -> Integer
getSizes [] = 0
getSizes ((_,e):es) = case (getType2 e) of
  QVal -> intSize+(getSizes es)
  _ -> ptrSize+(getSizes es)

flushBlock :: String -> GenerateQuadruplets ()
flushBlock name = do
  (nextL, nextV, block, blocks, structMap, fType) <- get
  put (nextL, nextV, (SimpleBlock name [] []), (block:blocks), structMap, fType)
  return ()

getStructElType :: String -> String -> GenerateQuadruplets QType
getStructElType className fieldName = do
  (_, _, _, _, sm, _) <- get
  case (Map.lookup className sm) of
    (Just c) -> do
      case Map.lookup fieldName c of
        (Just t) -> return $ getType2 t

addToBlock :: Quad -> GenerateQuadruplets ()
addToBlock q = do
  (nextLabel, nextTemp, (SimpleBlock name qs vars), blocks, structMap, fType) <- get
  put (nextLabel, nextTemp, (SimpleBlock name (qs ++ [q]) vars), blocks, structMap, fType)
  return ()

addLiveVar :: Var -> GenerateQuadruplets ()
addLiveVar v = do
  (nextLabel, nextTemp, (SimpleBlock name qs vars), blocks, structMap, fType) <- get
  put (nextLabel, nextTemp, (SimpleBlock name qs (v:vars)), blocks, structMap, fType)
  return ()

freshTemp :: GenerateQuadruplets String
freshTemp = do
  (nextLabel, nextTemp, block, blocks, structMap, fType) <- get
  let temp = "t" ++ (show nextTemp)
  put (nextLabel, nextTemp+1, block, blocks, structMap, fType)
  return temp

freshLabel :: GenerateQuadruplets String
freshLabel = do
  (nextLabel, nextTemp, block, blocks, structMap, fType) <- get
  let label = "L" ++ (show nextLabel)
  put (nextLabel+1, nextTemp, block, blocks, structMap, fType)
  return label

arrayType :: Var -> QType
arrayType (Var (QArrayRef t) _) = t
