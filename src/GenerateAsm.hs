module GenerateAsm where

import AbsLatte

import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State

import System.Random

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Data.Char (ord)

import Misc

import GenerateQuadruplets
import Quadruplets

data Loc = Const String | Reg String | Mem String
  deriving Show

type RegisterMapping = Map.Map String ([String], Bool) -- mapping from reg to (stored val, is val 8 bytes)
type LocationMapping = Map.Map String (Maybe Integer, Bool, [String]) -- (mem location, is in location, registers)
type StringLiterals = Map.Map String (Integer, Integer) -- string -> (address, reference count)
type Store = (StructMap, RegisterMapping, LocationMapping, StringLiterals, Integer, [String])
  -- last argument - stack usage
type Env = ()
type GenerateAsm a = ReaderT Env (ErrorT String (StateT Store IO)) a

runBackend :: GenerateAsm a -> IO (Either String a, Store)
runBackend ga = runStateT (runErrorT (runReaderT ga ())) (Map.empty, Map.empty, Map.empty, Map.empty, 0, [])

generateCode :: StructMap -> [TopDef PositionInfo] -> GenerateAsm ()
generateCode structMap functions = do
  let (err, _) = runGenerateQuadruplets (generateAll functions) structMap
  case err of
    Left errMsg -> throwError $ "Couldn't generate quadruplets" ++ errMsg
    Right simpleBlocks -> do
      (_, rm, lm, sl, i, emitted) <- get
      put (structMap, rm, lm, sl, i, emitted)
      emit2 $ ".text"
      emit2 $ ".globl main"
      generateTopDefs functions simpleBlocks
  return ()

generateTopDefs :: [TopDef a] -> [[SimpleBlock]] -> GenerateAsm ()
generateTopDefs [] _ = return ()
generateTopDefs (def:defs) (b:bs) = do
  generateTopDef def b
  generateTopDefs defs bs

generateTopDef :: TopDef a -> [SimpleBlock] -> GenerateAsm ()
generateTopDef (FnDef _ _ fName args _) simpleBlocks = do
  emit2 $ (getIdentString fName) ++ ":"
  emit2 $ "push %rbp"
  emit2 $ "mov %rsp, %rbp"
  argsMapping <- getArgsMapping 16 args
  (sm, _, _, _, _, _) <- get
  put (sm, Map.empty, argsMapping, Map.empty, 0, [])
  mapM_ generateBlock simpleBlocks
  (_, _, _, _, stack, emitted) <- get
  emit2 $ "sub $" ++ show (stack + (16-(mod stack 16))) ++ ",%rsp"
  mapM_ emit2 (reverse emitted)
  emit2 $ "leave"
  emit2 $ "ret"
  return ()

deleteDeadFromReg :: String -> Set.Set String -> GenerateAsm ()
deleteDeadFromReg reg liveVars = do
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  case Map.lookup reg rm of
    Just (elems, isLong) -> do
      let deadVars = filter ((flip Set.notMember) liveVars) elems
      mapM_ ((flip deleteVarFromReg) reg) deadVars
      return ()
    _ -> return ()
  return ()

getArgsMapping :: Integer -> [Arg a] -> GenerateAsm LocationMapping
getArgsMapping _ [] = return Map.empty
getArgsMapping currentOffset ((Arg _ t name):args) = do
  m <- getArgsMapping (currentOffset + 8) args
  return $ Map.insert (getIdentString name) (Just currentOffset, True, []) m

getTypeSize :: Type a -> Integer
getTypeSize (Int _) = intSize
getTypeSize (Bool _) = intSize
getTypeSize (Void _) = intSize
getTypeSize _ = ptrSize

generateBlock :: SimpleBlock -> GenerateAsm ()
generateBlock (SimpleBlock name quads liveVars) = do
  emit $ name ++ ":"
  mapM_ generateQuad $ generateLive (SimpleBlock name quads liveVars)
  saveLiveVars (Set.fromList (map getVarName liveVars))
  mapM_ clearRegister registers64
  return ()

generateQuad :: QuadWithLive -> GenerateAsm ()
generateQuad (QL q liveVars) = do
  generateQWithLive q liveVars
  mapM_ ((flip deleteDeadFromReg) liveVars) registers64
  return ()

generateQWithLive :: Quad -> Set.Set String -> GenerateAsm ()
generateQWithLive (Jump label) liveVars = do
  saveLiveVars liveVars
  emit $ "jmp " ++ label
generateQWithLive (CTJump v label) liveVars = generateJumpSingle v label "jne" liveVars
generateQWithLive (CFJump v label) liveVars = generateJumpSingle v label "je" liveVars
generateQWithLive (LTJump v1 v2 label) liveVars = generateJumpBin v1 v2 label "jl" liveVars
generateQWithLive (LEJump v1 v2 label) liveVars = generateJumpBin v1 v2 label "jle" liveVars
generateQWithLive (GTJump v1 v2 label) liveVars  = generateJumpBin v1 v2 label "jg" liveVars
generateQWithLive (GEJump v1 v2 label) liveVars = generateJumpBin v1 v2 label "jge" liveVars
generateQWithLive (EQJump v1 v2 label) liveVars = generateJumpBin v1 v2 label "je" liveVars
generateQWithLive (NEQJump v1 v2 label) liveVars = generateJumpBin v1 v2 label "jne" liveVars
generateQWithLive q _ = generateQ q

generateQ :: Quad -> GenerateAsm ()
generateQ VReturn = do
  emit $ "leave"
  emit $ "ret"
generateQ (QReturn v) = do
  (s, b) <- varLoc v
  reg <- getRegisterFromLoc s (getVarName v) b []
  if b then
    emit $ "mov " ++ reg ++ ",%rax"
  else
    emit $ "mov " ++ (registerConvert reg) ++ ",%eax"
  emit $ "leave"
  emit $ "ret"
  return ()
generateQ (ConstInt v i) = do
  reg <- requestRegister
  case v of
    (Var QVal name) -> do
      rememberVarOnly name reg False
      emit $ "mov $" ++ (show i) ++ ","++(registerConvert reg)
    (Var _ name) -> do
      rememberVarOnly name reg True
      emit $ "mov $" ++ (show i) ++ ","++reg
generateQ (ConstString v str) = do
  -- TODO: count string reference
  generateMalloc v (IConst (toInteger ((length str)+1)))
  copyLetters (deleteParenthesis str) 0
generateQ (StoreStruct struct field val) = do
  (sm, _, _, _, _, _) <- get
  (locStruct, _) <- varLoc struct
  structReg <- getRegisterFromLoc locStruct (getVarName struct) True []
  (locVal, valLong) <- varLoc val
  valReg <- getRegisterFromLoc locVal (getVarName val) valLong [structReg]
  let valReg2 = if valLong then valReg else (registerConvert valReg)
  let (Var (QClass structName) _) = struct
  let structMap = deleteJust (Map.lookup structName sm)
  let elementShift = structShift structMap field
  let mov = if ((varSize val) == ptrSize) then "movq " else "movl "
  emit $ mov ++ valReg2 ++ "," ++ (show elementShift) ++ "(" ++ structReg ++ ")"
  return ()
generateQ (StoreArray array index val) = do
  (locArray, _) <- varLoc array
  arrayReg <- getRegisterFromLoc locArray (getVarName array)  True []
  (locIndex, _) <- varLoc index
  indexReg <- constReg locIndex (getVarName index) False [arrayReg]
  (locVal, valLong) <- varLoc val
  valReg <- getRegisterFromLoc locVal (getVarName val)  valLong [arrayReg, indexReg]
  let valReg2 = if valLong then valReg else (registerConvert valReg)
  let arrayElSize = show (if valLong then ptrSize else intSize)
  let mov = if ((varSize val) == ptrSize) then "movq " else "movl "
  emit $ mov ++ valReg2 ++ ",4(" ++ arrayReg ++ "," ++ indexReg ++ "," ++ arrayElSize ++ ")"
  return ()
generateQ (Assign (Var _ varName) v2) = do
  (locV2, locV2Long) <- varLoc v2
  v2Reg <- case locV2 of
    (Const c) -> requestRegisterAndMoveLoc c Nothing locV2Long []
    _ -> getRegisterFromLoc locV2 (getVarName v2) locV2Long []
  rememberVarOnly varName v2Reg locV2Long
  return ()
generateQ (Malloc result size) = generateMalloc result size
generateQ (ArrayMalloc array size actualSize) = do
  generateMalloc array size
  (locSize, _) <- varLoc actualSize
  sizeReg <- getRegisterFromLoc locSize (getVarName actualSize) False []
  emit $ "movl " ++ (registerConvert sizeReg) ++ ",(%rax)"
generateQ (BinOp v1 v2 op v3) = do
  let forbidden = (if or [op == QDiv, op == QMod] then ["%rax", "%rdx"] else [])
  resultReg <- case op of
    QDiv -> do
      freeGivenRegister "%rax"
      freeGivenRegister "%rdx"
      return "%rax"
    QMod -> do
      freeGivenRegister "%rax"
      freeGivenRegister "%rdx"
      return "%rdx"
    _ -> requestRegister
  (locV2, locV2Long) <- varLoc v2
  val2Reg <- getRegisterFromLoc locV2 (getVarName v2) locV2Long (forbidden++[resultReg])
  (locV3, locV3Long) <- varLoc v3
  val3Reg <- getRegisterFromLoc locV3 (getVarName v3) locV3Long (forbidden++[resultReg, val2Reg])
  generateBinOp (registerConvert resultReg) (registerConvert val2Reg) op (registerConvert val3Reg)
  rememberVarOnly (getVarName v1) resultReg False
generateQ (Call fName res param) = do
  let params = reverse param
  paramStrings <- mapM varLoc params
  let paramStrings2 = map locString (map fst paramStrings)
  mapM_ freeGivenRegister registers64
  when (odd (length param)) (emit "sub $8,%rsp")
  mapM_ emitParam paramStrings2
  emit $ "call " ++ fName
  when (odd (length param)) (emit "add $8,%rsp")
  emit $ "add $" ++ (show (ptrSize*(toInteger (length params)))) ++ ",%rsp"
  rememberVarOnly (getVarName res) "%rax" (isVarLong res)
generateQ (Error) = do
  emit $ "call _Z5errorv"
  return ()
generateQ (PrintString str) = do
  freeGivenRegister "%rdi"
  (locStr, _) <- varLoc str
  strReg <- getRegisterFromLoc locStr (getVarName str) False ["%rdi"]
  emit $ "mov " ++ strReg ++ ",%rdi"
  mapM_ freeGivenRegister (List.delete "%rdi" registers64)
  emit $ "call _Z11printStringPKc"
generateQ (ReadStr res) = do
  mapM_ freeGivenRegister registers64
  emit $ "call _Z10readStringv"
  rememberVarOnly (getVarName res) "%rax" False
generateQ (PrintInt int) = do
  freeGivenRegister "%rdi"
  (locInt, _) <- varLoc int
  intReg <- getRegisterFromLoc locInt (getVarName int) False ["%rdi"]
  emit $ "mov " ++ (registerConvert intReg) ++ ",%edi"
  mapM_ freeGivenRegister (List.delete "%rdi" registers64)
  emit $ "call _Z8printInti"
generateQ (ReadInt res) = do
  mapM_ freeGivenRegister registers64
  emit $ "call _Z7readIntv"
  rememberVarOnly (getVarName res) "%rax" False
generateQ (ClassElem res struct field) = do
  (sm, _, _, _, _, _) <- get
  let (Var (QClass structName) _) = struct
  (locStruct, _) <- varLoc struct
  structReg <- getRegisterFromLoc locStruct (getVarName struct) True []
  let structMap = deleteJust (Map.lookup structName sm)
  let elementShift = structShift structMap field
  resReg <- requestRegisterWithout [structReg]
  let resReg2 = if ((varSize res) == ptrSize) then resReg else (registerConvert resReg)
  emit $ "mov " ++ (show elementShift) ++ "(" ++ structReg ++ ")," ++ resReg2
  rememberVarOnly (getVarName res) resReg ((varSize res) == ptrSize)
generateQ (ArrayElem res array index) = do
  (locArray, _) <- varLoc array
  arrayReg <- getRegisterFromLoc locArray (getVarName array) True []
  (locIndex, _) <- varLoc index
  indexReg <- constReg locIndex (getVarName index) False [arrayReg]
  let arrayElSize = varSize res
  resReg <- requestRegisterWithout [arrayReg, indexReg]
  let resReg2 = if ((varSize res) == ptrSize) then resReg else (registerConvert resReg)
  emit $ "mov 4(" ++ arrayReg ++ "," ++ indexReg ++ "," ++ (show arrayElSize) ++ ")," ++ resReg2
  rememberVarOnly (getVarName res) resReg ((varSize res) == ptrSize)
generateQ (Negate v1 v2) = do
  (locV2, _) <- varLoc v2
  v2Reg <- getRegisterFromLoc locV2 (getVarName v2) False []
  resReg <- requestRegisterWithout [v2Reg]
  emit $ "sub " ++ (registerConvert v2Reg) ++ "," ++ (registerConvert resReg)
  rememberVarOnly (getVarName v1) resReg False
generateQ (Concat res s1 s2) = do
  freeGivenRegister "%rdi"
  freeGivenRegister "%rsi"
  (s1L, _) <- varLoc s1
  (s2L, _) <- varLoc s2
  s1Reg <- getRegisterFromLoc s1L (getVarName s1) True ["%rdi", "%rsi"]
  s2Reg <- getRegisterFromLoc s2L (getVarName s2) True ["%rdi", "%rsi", s1Reg]
  emit $ "mov " ++ s1Reg ++ ",%rdi"
  emit $ "mov " ++ s2Reg ++ ",%rsi"
  mapM_ freeGivenRegister (List.delete "%rsi" (List.delete "%rdi" registers64))
  emit $ "call _Z6concatPcS_"
  rememberVarOnly (getVarName res) "%rax" True
generateQ (ArrayLen res array) = do
  (locArray, _) <- varLoc array
  arrayReg <- getRegisterFromLoc locArray (getVarName array) True []
  resReg <- requestRegisterWithout [arrayReg]
  emit $ "movl (" ++ arrayReg ++ ")," ++ (registerConvert resReg)
  rememberVarOnly (getVarName res) resReg False
generateQ (Increment v) = generateIncrement v 1
generateQ (Decrement v) = generateIncrement v (-1)
generateQ (IncrementField struct field) = generateIncrementField struct field 1
generateQ (DecrementField struct field) = generateIncrementField struct field (-1)
generateQ (IncrementArr array index) = generateIncrementArr array index 1
generateQ (DecrementArr array index) = generateIncrementArr array index (-1)

generateMalloc :: Var -> Var -> GenerateAsm ()
generateMalloc result size = do
  freeGivenRegister "%rdi"
  (locSize, _) <- varLoc size
  sizeReg <- getRegisterFromLoc locSize (getVarName size) False ["%rdi"]
  emit $ "mov " ++ (registerConvert sizeReg) ++ ",%edi"
  mapM_ freeGivenRegister (List.delete "%rdi" registers64)
  emit $ "call _Z5alloci"
  rememberVarOnly (getVarName result) "%rax" True

generateIncrement :: Var -> Integer -> GenerateAsm ()
generateIncrement v i = do
  (loc, locLong) <- varLoc v
  reg <- getRegisterFromLoc loc (getVarName v) locLong []
  let r = if ((varSize v) == ptrSize) then reg else (registerConvert reg)
  rememberVarOnly (getVarName v) reg ((varSize v) == ptrSize)
  emit $ "add $" ++ (show i) ++ "," ++ r

generateIncrementField :: Var -> String -> Integer -> GenerateAsm ()
generateIncrementField struct field i = do
  (sm, _, _, _, _, _) <- get
  (locStruct, _) <- varLoc struct
  structReg <- getRegisterFromLoc locStruct (getVarName struct) True []
  let (Var (QClass structName) _) = struct
  let structMap = deleteJust (Map.lookup structName sm)
  let elementShift = structShift structMap field
  emit $ "add $" ++ (show i) ++ "," ++ (show elementShift) ++ "(" ++ structReg ++ ")"
  return ()

generateIncrementArr :: Var -> Var -> Integer -> GenerateAsm ()
generateIncrementArr array index i = do
  (locArray, _) <- varLoc array
  arrayReg <- getRegisterFromLoc locArray (getVarName array)  True []
  (locIndex, _) <- varLoc index
  indexReg <- constReg locIndex (getVarName index) False [arrayReg]
  let arrayElSize = show (arrayElemSize array)
  emit $ "add $" ++ (show i) ++ ",4(" ++ arrayReg ++ "," ++ indexReg ++ "," ++ arrayElSize ++ ")"
  return ()

generateJumpSingle :: Var -> String -> String -> Set.Set String -> GenerateAsm ()
generateJumpSingle v label jumpName liveVars = do
  (locCond, _) <- varLoc v
  condReg <- constReg locCond (getVarName v) False []
  saveLiveVars liveVars
  emit $ "test " ++ (registerConvert condReg) ++ "," ++ (registerConvert condReg)
  emit $ jumpName ++ " " ++ label

generateJumpBin :: Var -> Var -> String -> String -> Set.Set String ->  GenerateAsm ()
generateJumpBin v1 v2 label jumpName liveVars = do
  (locV1, longV1) <- varLoc v1
  regV1 <- constReg locV1 (getVarName v1) longV1 []
  let r1 = if longV1 then regV1 else (registerConvert regV1)
  (locV2, longV2) <- varLoc v2
  regV2 <- getRegisterFromLoc locV2 (getVarName v2) longV2 [regV1]
  let r2 = if longV1 then regV2 else (registerConvert regV2)
  saveLiveVars liveVars
  emit $ "cmp " ++ r2 ++ "," ++ r1
  emit $ jumpName ++ " " ++ label

saveLiveVars :: Set.Set String -> GenerateAsm ()
saveLiveVars vars = mapM_ saveLiveVar (Set.toList vars)

saveLiveVar :: String -> GenerateAsm ()
saveLiveVar v = do
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  case Map.lookup v lm of
    Just ((Just _), True, _) -> return () -- var already in memory
    _ -> do
      regLoc <- findRegLocation v
      case regLoc of
        Just reg -> saveVar v reg
        _ -> return ()

generateBinOp :: String -> String -> BinOp -> String -> GenerateAsm ()
generateBinOp resReg v1 QPlus v2  = do
  emit $ "mov " ++ v1 ++ "," ++ resReg
  emit $ "add " ++ v2 ++ "," ++ resReg
generateBinOp resReg v1 QMinus v2  = do
  emit $ "mov " ++ v1 ++ "," ++ resReg
  emit $ "sub " ++ v2 ++ "," ++ resReg
generateBinOp resReg v1 QTimes v2  = do
  emit $ "mov " ++ v1 ++ "," ++ resReg
  emit $ "imul " ++ v2 ++ "," ++ resReg
generateBinOp resReg v1 QDiv v2  = performDiv resReg v1 v2
generateBinOp resReg v1 QMod v2 = performDiv resReg v1 v2

performDiv :: String -> String -> String -> GenerateAsm ()
performDiv resReg v1 v2 = do
  emit $ "mov " ++ v1 ++ ",%eax"
  emit $ "cdq"
  v2New <- case v2 of
    ('$':c) -> do
      r <- requestRegisterAndMoveLoc ('$':c) Nothing False ["%rax", "%rdx"]
      return $ registerConvert r
    _ -> return v2
  emit $ "idiv " ++ v2New

-- get const and move it to reg
constReg :: Loc -> String -> Bool -> [String] -> GenerateAsm String
constReg loc varName long forbidden = do
  case loc of
    (Const c) -> requestRegisterAndMoveLoc c Nothing long forbidden
    _ -> getRegisterFromLoc loc varName long forbidden

getRegisterFromLoc :: Loc -> String -> Bool -> [String] -> GenerateAsm String
getRegisterFromLoc loc varName isLong forbidden = do
  case loc of
    Reg reg -> return reg
    Mem l -> requestRegisterAndMoveLoc l (Just varName) isLong forbidden
    Const c -> return c

requestRegisterAndMoveLoc :: String -> Maybe String -> Bool -> [String] -> GenerateAsm String
requestRegisterAndMoveLoc loc varName isVarLong forbidden = do
  reg <- requestRegisterWithout forbidden
  emit $ "mov " ++ loc ++ "," ++ (if isVarLong then reg else (registerConvert reg))
  case varName of
    (Just varName) -> rememberVar varName reg isVarLong
    _ -> return ()
  return $ reg

structShift :: Struct -> String -> Integer
structShift struct field = structShiftList (Map.toList struct) field 0

structShiftList :: [(String, T)] -> String -> Integer -> Integer
structShiftList ((name, t):elems) fieldName currentShift =
  if name == fieldName then currentShift
  else structShiftList elems fieldName (currentShift+(typeSize (getType2 t)))

rememberVarOnly :: String -> String -> Bool -> GenerateAsm ()
rememberVarOnly varName regName isLong = do
  forgetVar varName isLong
  rememberVar varName regName isLong

forgetVar :: String -> Bool -> GenerateAsm ()
forgetVar varName isLong = do
  mapM_ (deleteVarFromReg varName) registers64
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  case Map.lookup varName lm of
    Just (loc, _, _) -> put (sm, rm, (Map.insert varName (loc, False, []) lm), sl, stackPtr, emitted)
    _ -> put (sm, rm, (Map.insert varName (Nothing, False, []) lm), sl, stackPtr, emitted)
  return ()

-- remember that var is saved in register
rememberVar :: String -> String -> Bool -> GenerateAsm  ()
rememberVar varName regName isLong = do
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  case Map.lookup regName rm of
    Just (vars, _) -> put (sm, (Map.insert regName ((varName:vars), isLong) rm), lm, sl, stackPtr, emitted)
    _ -> put (sm, (Map.insert regName ([varName], isLong) rm), lm, sl, stackPtr, emitted)
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  case Map.lookup varName lm of
    (Just (l1, l2, regs)) -> put (sm, rm, (Map.insert varName (l1, l2, (regName:regs)) lm), sl, stackPtr, emitted)
    _ -> put (sm, rm, (Map.insert varName (Nothing, False, [regName]) lm), sl, stackPtr, emitted)
  return ()

requestRegister :: GenerateAsm String
requestRegister = requestRegisterWithout []

-- requests a registers
requestRegisterWithout :: [String] -> GenerateAsm String
requestRegisterWithout forbidden = do
  (_, rm, _, _, _, _) <- get
  case findFreeRegister forbidden registers64 rm of
    Just reg -> do
      emit $ "xor " ++ reg ++ "," ++ reg
      return reg
    Nothing -> do
      reg <- freeRegisterWithout forbidden
      emit $ "xor " ++ reg ++ "," ++ reg
      return reg

freeRegisterWithout :: [String] -> GenerateAsm String
freeRegisterWithout forbidden = do
  reg <- randomRegWithout forbidden
  freeGivenRegister reg
  return reg

-- make register free
freeRegister :: GenerateAsm String
freeRegister = freeRegisterWithout []

clearRegister :: String -> GenerateAsm ()
clearRegister reg = do
  (sm, rm, lm, sl, stackPtr, emitted) <-get
  case Map.lookup reg rm of
    (Just (vars, _)) -> mapM_ ((flip deleteVarFromReg) reg) vars
    _ -> return ()

freeGivenRegister :: String -> GenerateAsm ()
freeGivenRegister reg = do
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  case Map.lookup reg rm of
    (Just (vars, _)) -> saveVarsAndDelete vars reg
    Nothing -> return ()

saveVarsAndDelete :: [String] -> String -> GenerateAsm ()
saveVarsAndDelete [] _ = return ()
saveVarsAndDelete (var:vars) regName = do
  saveVar var regName
  deleteVarFromReg var regName
  saveVarsAndDelete vars regName

deleteVarFromReg :: String -> String -> GenerateAsm ()
deleteVarFromReg varName regName = do
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  case (Map.lookup varName lm) of
    Nothing -> return ()
    Just (l1, l2, regs) -> do
      let regs2 = deleteAll regName regs
      case Map.lookup regName rm of
        Nothing -> return ()
        Just (vars, long) -> do
          let vars2 = deleteAll varName vars
          put (sm, (Map.insert regName (vars2, long) rm), (Map.insert varName (l1, l2, regs2) lm), sl, stackPtr, emitted)
          return ()

-- move variable from register to memory
saveVar :: String -> String -> GenerateAsm ()
saveVar varName regName = do
  (sm, rm, lm, sl, stackPtr, emitted) <- get
  let varLocs = deleteJust (Map.lookup varName lm)
  let (_, long) = deleteJust (Map.lookup regName rm)
  case varLocs of
    (Nothing, _, regs) -> do
      let stackPtr2 = stackPtr + (if long then ptrSize else intSize)
      if long then do
        emit $ "mov " ++ regName ++ "," ++ (show (-1*stackPtr2)) ++ "(%rbp)"
      else do
        emit $ "mov " ++ (registerConvert regName) ++ "," ++ (show (-1*stackPtr2)) ++ "(%rbp)"
      let varLocks2 = ((Just (-1*stackPtr2)), True, regs)
      (sm, rm, lm, sl, stackPtr, emitted) <- get
      put (sm, rm, (Map.insert varName varLocks2 lm), sl, stackPtr2, emitted)
      return ()
    ((Just loc), False, regs) -> do
      let regName2 = if long then regName else (registerConvert regName)
      emit $ "mov " ++ regName2 ++ "," ++ (show loc) ++ "(%rbp)"
      let varLocks2 = ((Just loc), True, regs)
      (sm, rm, lm, sl, stackPtr, emitted) <- get
      put (sm, rm, (Map.insert varName varLocks2 lm), sl, stackPtr, emitted)
    _ -> return ()

-- looks for a free register
findFreeRegister :: [String] -> [String] -> RegisterMapping -> Maybe String
findFreeRegister _ [] _ = Nothing
findFreeRegister forbidden (reg:regs) rm =
  if (reg `elem` forbidden) then findFreeRegister forbidden regs rm
  else
    case Map.lookup reg rm of
      Nothing -> Just reg
      (Just ([], _)) -> Just reg
      _ -> findFreeRegister forbidden regs rm

-- string for variable, does variable require long
varLoc :: Var -> GenerateAsm (Loc, Bool)
varLoc (IConst i) = return (Const ("$" ++ (show i)), False)
varLoc (Null) = return (Const "$0", True)
varLoc (QTrue) = return (Const "$1", False)
varLoc (QFalse) = return (Const "$0", False)
varLoc (Var QVal name) = do
  loc <- varLoc2 (Var QVal name)
  return (loc, False)
varLoc (Var t name) = do
  loc <- varLoc2 (Var t name)
  return (loc, True)

varLoc2 :: Var -> GenerateAsm Loc
varLoc2 (Var _ name) = do
  regLoc <- findRegLocation name
  case regLoc of
    Nothing -> do
      (_, _, lm, _, _, _) <- get
      case Map.lookup name lm of
        Just ((Just addr), True, _) -> return $ Mem ((show addr) ++ "(%rbp)")
        _ -> throwError $ "Error while looking for var " ++ name ++ "in memory"
    Just regName -> return $ Reg regName

findRegLocation :: String -> GenerateAsm (Maybe String)
findRegLocation name = do
  (_, _, lm, _, _, _) <- get
  case Map.lookup name lm of
    Just (_, _, (reg:_)) -> return $ Just reg
    _ -> return Nothing

generateLive :: SimpleBlock -> [QuadWithLive]
generateLive (SimpleBlock _ quads liveAfterBlock) =
  reverse (generateLiveSingle (Set.fromList (map getVarName liveAfterBlock)) (reverse quads))

generateLiveSingle :: (Set.Set String) -> [Quad] -> [QuadWithLive]
generateLiveSingle _ [] = []
generateLiveSingle after (q:qs) = ((QL q after):(generateLiveSingle (lifeBefore after q) qs))

lifeBefore :: Set.Set String -> Quad -> Set.Set String
lifeBefore after (ConstInt v _) = setDeleteVar v after
lifeBefore after (ConstString v _) = setDeleteVar v after
lifeBefore after (StoreStruct struct _ val) =
  setInsertVar val (setInsertVar struct after)
lifeBefore after (StoreArray v1 v2 v3) =
  setInsertVar v1 (setInsertVar v2 (setInsertVar v3 after))
lifeBefore after (Assign v1 v2) =
  setInsertVar v2 (setDeleteVar v1 after)
lifeBefore after (Malloc v1 v2) =
  setInsertVar v2 (setDeleteVar v1 after)
lifeBefore after (ArrayMalloc v1 v2 v3) =
  setInsertVar v3 (setInsertVar v2 (setDeleteVar v1 after))
lifeBefore after (BinOp v1 v2 _ v3) =
  setInsertVar v2 (setInsertVar v3 (setDeleteVar v1 after))
lifeBefore after (Call _ v ps) = foldl (flip setInsertVar) (setDeleteVar v after) ps
lifeBefore after (ClassElem v1 v2 _) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (ArrayElem v1 v2 v3) = setInsertVar v2 (setInsertVar v3 (setDeleteVar v1 after))
lifeBefore after (Negate v1 v2) = setInsertVar v2 (setDeleteVar v1 after)
lifeBefore after (Concat v1 v2 v3) = setInsertVar v2 (setInsertVar v3 (setDeleteVar v1 after))
lifeBefore after (CTJump v1 _) = setInsertVar v1 after
lifeBefore after (CFJump v1 _) = setInsertVar v1 after
lifeBefore after (LTJump v1 v2 _) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (LEJump v1 v2 _) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (GTJump v1 v2 _) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (GEJump v1 v2 _) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (EQJump v1 v2 _) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (NEQJump v1 v2 _) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (QReturn v) = setInsertVar v after
lifeBefore after (ArrayLen v1 v2) = setInsertVar v2 (setDeleteVar v1 after)
lifeBefore after (Increment v) = setDeleteVar v after
lifeBefore after (Decrement v) = setDeleteVar v after
lifeBefore after (IncrementField v _) = setInsertVar v after
lifeBefore after (DecrementField v _) = setInsertVar v after
lifeBefore after (IncrementArr v1 v2) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (DecrementArr v1 v2) = setInsertVar v1 (setInsertVar v2 after)
lifeBefore after (PrintString v) = setInsertVar v after
lifeBefore after (PrintInt v) = setInsertVar v after
lifeBefore after (ReadInt v) = setDeleteVar v after
lifeBefore after (ReadStr v) = setDeleteVar v after
lifeBefore after _ = after

setInsertVar :: Var -> Set.Set String -> Set.Set String
setInsertVar v s = Set.insert (getVarName v) s

setDeleteVar :: Var -> Set.Set String -> Set.Set String
setDeleteVar v s = Set.delete (getVarName v) s

getVarName :: Var -> String
getVarName (Var _ name) = name
getVarName _ = ""

registers64 :: [String]
registers64 = ["%rax", "%rbx", "%rcx", "%rdx", "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15"]

registerConvert :: String -> String
registerConvert "rax" = "eax"
registerConvert "rbx" = "ebx"
registerConvert "rcx" = "ecx"
registerConvert "rdx" = "edx"
registerConvert "r9" = "r9d"
registerConvert "r10" = "r10d"
registerConvert "r11" = "r11d"
registerConvert "r12" = "r12d"
registerConvert "r13" = "r13d"
registerConvert "r14" = "r14d"
registerConvert "r15" = "r15d"
registerConvert ('$':r) = ('$':r) -- converting const
registerConvert ('%':r) = ('%':registerConvert r) -- converting %reg

emit :: String -> GenerateAsm ()
emit s = do
  if s `elem` ["mov %eax,%eax", "sub $0,%esp", "add $0,%esp"] then
    return ()
  else do
    (sm, rm, lm, sl, stack, emitted) <- get
    put (sm, rm, lm, sl, stack, s:emitted)
    return ()

emit2 :: String -> GenerateAsm ()
emit2 s = do
  liftIO $ putStrLn $ s
  return ()

deleteJust :: Maybe a -> a
deleteJust (Just x) = x

-- Based on: https://stackoverflow.com/a/25923941
randomRegWithout :: [String] -> GenerateAsm String
randomRegWithout forbidden = do
    let newList = foldl (flip List.delete) registers64 forbidden
    i <- liftIO $ randomRIO (0, length newList-1)
    return $ newList !! i

deleteAll :: Eq a => a -> [a] -> [a]
deleteAll _ [] = []
deleteAll el (x:els) = if x == el then deleteAll el els
  else (x:(deleteAll el els))

arrayElemSize :: Var -> Integer
arrayElemSize (Var (QArrayRef t) _) = typeSize t

varSize :: Var -> Integer
varSize (Var t _) = typeSize t
varSize Null = ptrSize
varSize _ = intSize

typeSize :: QType -> Integer
typeSize QVal = intSize
typeSize _ = ptrSize

locString :: Loc -> String
locString (Reg l) = l
locString (Mem l) = l
locString (Const l) = l

emitParam :: String -> GenerateAsm ()
emitParam param = do
  emit $ "pushq " ++ param

isVarLong :: Var -> Bool
isVarLong Null = True
isVarLong (Var t _) = (typeSize t) == ptrSize
isVarLong _ = False

copyLetters :: String -> Integer -> GenerateAsm ()
copyLetters [] shift = emit $ "movb $0," ++ (show shift) ++ "(%rax)"
copyLetters (s:ss) shift = do
  emit $ "movb $" ++ (show (ord s)) ++ "," ++ (show shift) ++ "(%rax)"
  copyLetters ss (1+shift)

deleteParenthesis :: String -> String
deleteParenthesis [] = []
deleteParenthesis (_:s) = take ((length s)-1) s
