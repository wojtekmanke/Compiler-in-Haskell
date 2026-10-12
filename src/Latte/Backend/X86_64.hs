{-# LANGUAGE LambdaCase #-}

-- | Generation of x86-64 assembly from quadruples.
--
-- Registers are allocated locally, one basic block at a time, in the manner
-- of the simple code generator of the Dragon Book: the generator tracks, for
-- every register, the variables it currently holds and, for every variable,
-- whether its stack slot is up to date. A value is loaded into a register
-- when it is first needed and is written back to its slot only if it is still
-- live when its register is taken away, at a call, or at the end of the block.
-- Between blocks, every live variable is in its stack slot and no register
-- holds anything.
--
-- Every variable has an 8-byte slot in the frame of its function, whatever
-- its size; "Latte.Layout" describes how values are represented.
--
-- Calling convention between Latte functions: arguments are pushed on the
-- stack, the first one last, and popped by the caller; the result is returned
-- in @%rax@. Only caller-saved registers are ever allocated, so a function
-- has nothing to preserve besides @%rbp@, and the generated code can be
-- called from C. Functions of the runtime library are called according to the
-- System V ABI.
module Latte.Backend.X86_64 (generateAssembly) where

import Control.Monad (forM_, unless, when, zipWithM_, (>=>))
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Control.Monad.State (State, execState, gets, modify)
import Data.List (mapAccumL, minimumBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Set as Set

import Latte.Backend.Asm (Arg (..), Line (..), Reg (..), jumpTarget, render, stringLiteral)
import qualified Latte.Backend.Asm as Asm
import Latte.Error (internalError)
import Latte.Layout
import Latte.Quadruples
import Latte.Quadruples.Liveness
import Latte.Types (Builtin (..))

type Gen = ReaderT Context (State CodeGen)

-- | What the quad or terminator being translated has to preserve.
data Context = Context
  { liveNow :: Set.Set Var  -- ^ The variables that are live after it.
  , needed  :: Set.Set Var  -- ^ Those variables and its own operands.
  }

-- | The state of code generation. Each field describes the program, the
-- function, the block or the quad being translated, and starts afresh with
-- the next one.
data CodeGen = CodeGen
  { strings   :: Map.Map String Int
    -- ^ Program: its string literals, numbered in order of appearance.
  , slots     :: Map.Map Var Int
    -- ^ Function: the offset from @%rbp@ of the stack slot of each variable
    -- that has one.
  , slotCount :: Int
    -- ^ Function: the number of slots allocated for local variables.
  , code      :: [Line]
    -- ^ Function: its body so far, in reverse.
  , registers :: Map.Map Reg (Set.Set Var)
    -- ^ Block: the variables whose current value each register holds. A
    -- variable is in at most one register; copies of each other share one.
  , inMemory  :: Set.Set Var
    -- ^ Block: the variables whose stack slot holds their current value.
  , pinned    :: [Reg]
    -- ^ Quad: the registers its translation is using, which therefore
    -- cannot be given to anything else.
  }

-- | The registers values are kept in. All of them are caller-saved; @%rdi@
-- and @%rsi@ are left out because they carry the arguments of runtime calls.
allocatable :: [Reg]
allocatable = [RAX, RCX, RDX, R8, R9, R10, R11]

generateAssembly :: [Function] -> String
generateAssembly functions = render $
  [Directive ".text", Directive (".globl\t" ++ functionSymbol "main")]
    ++ concat bodies
    ++ stringSection literals
    ++ [Blank, Directive ".section\t.note.GNU-stack,\"\",@progbits"]
  where
    (literals, bodies) = mapAccumL genFunction Map.empty functions

stringSection :: Map.Map String Int -> [Line]
stringSection literals
  | Map.null literals = []
  | otherwise = Blank : Directive ".section\t.rodata" : concatMap entry ordered
  where
    ordered = sortOn snd (Map.toList literals)
    entry (text, index) = [LabelDef (stringSymbol index), stringLiteral text]

-- * Names

stringSymbol :: Int -> String
stringSymbol index = ".LC" ++ show index

labelSymbol :: Label -> String
labelSymbol (Label number) = ".L" ++ show number

-- | The symbol of a Latte function. The prefix keeps the functions of a
-- program apart from those of the C library; an apostrophe, which is legal in
-- a Latte identifier but not in a symbol, becomes a dot.
functionSymbol :: String -> String
functionSymbol name = "latte_" ++ map (\char -> if char == '\'' then '.' else char) name

-- | The symbols of the runtime library. Their prefix differs from that of
-- Latte functions, so the two cannot clash.
builtinSymbol :: Builtin -> String
builtinSymbol = \case
  PrintInt    -> "lrt_print_int"
  PrintString -> "lrt_print_string"
  ReadInt     -> "lrt_read_int"
  ReadString  -> "lrt_read_string"
  Error       -> "lrt_error"

supportSymbol :: Support -> String
supportSymbol = \case
  Concat       -> "lrt_concat"
  StringEquals -> "lrt_string_equals"
  NewObject    -> "lrt_new_object"
  NewArray     -> "lrt_new_array"

-- * Functions and blocks

-- | Translates a function, given the string literals seen so far and
-- returning them together with those of the function.
genFunction :: Map.Map String Int -> Function -> (Map.Map String Int, [Line])
genFunction literals (Function name params blocks) =
  (strings final, prologue ++ filter isUsed body)
  where
    following = map (Just . blockLabel) (drop 1 blocks) ++ [Nothing]
    translation = sequence_ (zipWith3 genBlock blocks (liveness blocks) following)
    final = execState (runReaderT translation (Context Set.empty Set.empty)) CodeGen
      { strings   = literals
      , slots     = Map.fromList (zip params [16, 24 ..])  -- above the return address
      , slotCount = 0
      , code      = []
      , registers = Map.empty
      , inMemory  = Set.empty
      , pinned    = []
      }
    body = reverse (code final)
    frame = 16 * ((slotCount final + 1) `div` 2)  -- keeps %rsp aligned to 16 bytes
    prologue =
      [ Blank
      , LabelDef (functionSymbol name)
      , Code (Asm.Push (Register RBP))
      , Code (Asm.Mov W64 (Register RSP) (Register RBP))
      ]
        ++ [Code (Asm.Sub W64 (Imm (toInteger frame)) (Register RSP)) | frame > 0]
    -- Labels that nothing jumps to are left out.
    targets = Set.fromList (mapMaybe jumpTarget body)
    isUsed = \case
      LabelDef symbol -> symbol `Set.member` targets
      _               -> True

-- | Translates a block, given what is live in it and the block laid out
-- after it, if any.
genBlock :: BasicBlock -> Live -> Maybe Label -> Gen ()
genBlock (BasicBlock name quads end) live next = do
  modify $ \state -> state
    { registers = Map.empty
    , inMemory  = liveIn live
    , code      = LabelDef (labelSymbol name) : code state
    }
  zipWithM_ (\quad after -> step after (uses quad) (genQuad quad)) quads (liveAfter live)
  step (liveOut live) (terminatorUses end) (genTerminator end next)

-- | Runs the translation of one quad or terminator, given what is live after
-- it and what it reads.
step :: Set.Set Var -> [Var] -> Gen () -> Gen ()
step after operands translate = do
  local (const (Context after (after `Set.union` Set.fromList operands))) translate
  -- Its registers are free again, and dead values no longer occupy any.
  modify $ \state -> state
    { registers = Map.map (Set.intersection after) (registers state)
    , pinned    = []
    }

genTerminator :: Terminator -> Maybe Label -> Gen ()
genTerminator end next = case end of
  Goto target -> do
    saveLive
    unless (next == Just target) $ emit (Asm.Jmp (labelSymbol target))
  Branch comparison left right onTrue onFalse -> do
    reg <- inRegister left
    source <- use right
    saveLive  -- writes to stack slots only, so the operands stay where they are
    emit (Asm.Cmp (operandWidth left) source (Register reg))
    if next == Just onTrue
      then emit (Asm.Jcc (negateComparison comparison) (labelSymbol onFalse))
      else do
        emit (Asm.Jcc comparison (labelSymbol onTrue))
        unless (next == Just onFalse) $ emit (Asm.Jmp (labelSymbol onFalse))
  Return value -> do
    forM_ value $ \operand -> do
      source <- use operand
      move (operandWidth operand) source (Register RAX)
    emit Asm.Leave
    emit Asm.Ret
  where
    saveLive = asks liveNow >>= save

-- * Quads

genQuad :: Quad -> Gen ()
genQuad = \case
  Assign target (Ref source)
    | source == target -> return ()
    | otherwise -> do
        -- A copy costs no instruction: both variables share the register.
        reg <- inRegister (Ref source)
        modify $ \state -> state
          { registers = Map.insertWith Set.union reg (Set.singleton target) (forget target state)
          , inMemory  = Set.delete target (inMemory state)
          }
  Assign target (Const _ value) -> do
    reg <- allocate
    move (width target) (Imm value) (Register reg)
    define reg target
  LoadString target text -> do
    symbol <- stringLiteralSymbol text
    reg <- allocate
    emit (Asm.Lea symbol reg)
    define reg target
  BinOp target op left right -> case op of
    Add       -> arithmetic Asm.Add
    Subtract  -> arithmetic Asm.Sub
    Multiply  -> arithmetic Asm.Imul
    Divide    -> divide RAX target left right
    Remainder -> divide RDX target left right
    where
      arithmetic instruction = do
        source <- use right
        reg <- scratch target left
        emit (instruction W32 source (Register reg))
        define reg target
  Negate target operand -> do
    reg <- scratch target operand
    emit (Asm.Neg W32 (Register reg))
    define reg target
  Load target memory -> do
    address <- addressOf memory
    reg <- allocate
    move (width target) address (Register reg)
    define reg target
  Store memory value -> do
    address <- addressOf memory
    -- There is no move from memory to memory.
    source <- use value >>= \case
      Memory _ _ -> Register <$> inRegister value
      direct     -> return direct
    move (widthOf (memType memory)) source address
  Bump memory amount -> do
    address <- addressOf memory
    emit (Asm.Add W32 (Imm amount) address)
  Call result callee args -> do
    case callee of
      Defined name    -> callFunction (functionSymbol name) args
      Builtin builtin -> callRuntime (builtinSymbol builtin) args
      Support support -> callRuntime (supportSymbol support) args
    forM_ result (define RAX)

-- | Calls a Latte function: the arguments go on the stack, the first one last.
callFunction :: String -> [Operand] -> Gen ()
callFunction symbol args = do
  -- %rsp is a multiple of 16 here and has to be one again at the call.
  let padding = length args `mod` 2
      pushed = length args + padding
  when (padding > 0) $ emit (Asm.Sub W64 (Imm 8) (Register RSP))
  forM_ (reverse args) (use >=> emit . Asm.Push)
  saveAcrossCall
  emit (Asm.Call symbol)
  when (pushed > 0) $ emit (Asm.Add W64 (Imm (toInteger (8 * pushed))) (Register RSP))

-- | Calls a function of the runtime library, which takes at most two
-- arguments, in registers.
callRuntime :: String -> [Operand] -> Gen ()
callRuntime symbol args = do
  forM_ (zip [RDI, RSI] args) $ \(reg, arg) -> do
    source <- use arg
    move (operandWidth arg) source (Register reg)
  saveAcrossCall
  emit (Asm.Call symbol)

-- | Translates a division. @idivl@ divides @%edx:%eax@ by its operand and
-- leaves the quotient in @%eax@ and the remainder in @%edx@; the caller says
-- which of the two is the result.
divide :: Reg -> Var -> Operand -> Operand -> Gen ()
divide result target left right = do
  dividendHolder <- registerOfOperand left
  divisorHolder <- registerOfOperand right
  -- A dividend that is already in %eax stays there. The division overwrites
  -- it, so it is saved only if it is still live afterwards.
  let inPlace = dividendHolder == Just RAX && divisorHolder /= Just RAX
  asks (if inPlace then liveNow else needed) >>= release RAX
  asks needed >>= release RDX
  pin RAX
  pin RDX
  divisor <- use right >>= \case
    Imm value -> do
      -- An immediate cannot be divided by.
      reg <- allocate
      move W32 (Imm value) (Register reg)
      return (Register reg)
    other -> return other
  unless inPlace $ do
    dividend <- use left
    move W32 dividend (Register RAX)
  emit Asm.Cltd
  emit (Asm.Idiv W32 divisor)
  define result target

-- | The operand that denotes a memory location.
addressOf :: Mem -> Gen Arg
addressOf = \case
  Member object field -> do
    base <- inRegister (Ref object)
    return (Memory (fieldOffset field) base)
  Length array -> do
    base <- inRegister (Ref array)
    return (Memory 0 base)
  Element array index element -> do
    base <- inRegister (Ref array)
    let scale = bytes (widthOf element)
    case index of
      -- A small constant index folds into the displacement.
      Const _ value | 0 <= value && value < 0x10000 ->
        return (Memory (arrayHeader + fromInteger value * scale) base)
      _ -> do
        reg <- inRegister index
        -- The upper half of a register holding an `int` is unspecified.
        emit (Asm.Movslq reg)
        return (Indexed arrayHeader base reg scale)

-- * Where values are

width :: Var -> Width
width = widthOf . varType

operandWidth :: Operand -> Width
operandWidth = widthOf . operandType

emit :: Asm.Instruction -> Gen ()
emit instruction = modify $ \state -> state { code = Code instruction : code state }

-- | Copies a value. A move of something to itself is left out.
move :: Width -> Arg -> Arg -> Gen ()
move size source target = unless (source == target) $ emit (Asm.Mov size source target)

registerOf :: Var -> Gen (Maybe Reg)
registerOf var = gets $ \state ->
  listToMaybe [reg | (reg, held) <- Map.toList (registers state), var `Set.member` held]

registerOfOperand :: Operand -> Gen (Maybe Reg)
registerOfOperand = \case
  Ref var   -> registerOf var
  Const _ _ -> return Nothing

-- | The register contents with a variable removed from wherever it was.
forget :: Var -> CodeGen -> Map.Map Reg (Set.Set Var)
forget var = Map.map (Set.delete var) . registers

-- | The stack slot of a variable, allocated on first use.
slot :: Var -> Gen Arg
slot var = do
  known <- gets (Map.lookup var . slots)
  offset <- case known of
    Just offset -> return offset
    Nothing -> do
      count <- gets slotCount
      let offset = -8 * (count + 1)
      modify $ \state -> state
        { slots     = Map.insert var offset (slots state)
        , slotCount = count + 1
        }
      return offset
  return (Memory offset RBP)

-- | Records that a register now holds the newly computed value of a variable
-- and nothing else.
define :: Reg -> Var -> Gen ()
define reg var = modify $ \state -> state
  { registers = Map.insert reg (Set.singleton var) (forget var state)
  , inMemory  = Set.delete var (inMemory state)
  }

-- | Writes to their stack slots those of the given variables that are only
-- in a register.
save :: Set.Set Var -> Gen ()
save vars = forM_ (Set.toList vars) $ \var -> do
  stored <- gets (Set.member var . inMemory)
  holder <- registerOf var
  case holder of
    Just reg | not stored -> do
      target <- slot var
      move (width var) (Register reg) target
      modify $ \state -> state { inMemory = Set.insert var (inMemory state) }
    _ -> return ()

-- | Empties a register, first saving those of the given variables it holds.
release :: Reg -> Set.Set Var -> Gen ()
release reg keep = do
  held <- gets (Map.findWithDefault Set.empty reg . registers)
  save (held `Set.intersection` keep)
  modify $ \state -> state { registers = Map.delete reg (registers state) }

-- | A call may overwrite every register: saves what is live after it.
saveAcrossCall :: Gen ()
saveAcrossCall = do
  asks liveNow >>= save
  modify $ \state -> state { registers = Map.empty }

-- * Registers
--
-- Every register that the functions below hand out is pinned: it will not be
-- handed out again, or emptied, until the current quad has been translated.

pin :: Reg -> Gen ()
pin reg = modify $ \state -> state { pinned = reg : pinned state }

-- | Where the value of an operand can be read: an immediate, a register or
-- a stack slot.
use :: Operand -> Gen Arg
use = \case
  Const _ value -> return (Imm value)
  Ref var -> registerOf var >>= \case
    Just reg -> Register reg <$ pin reg
    Nothing -> do
      stored <- gets (Set.member var . inMemory)
      if stored
        then slot var
        else internalError ("variable " ++ show (varId var) ++ " is read before it is assigned")

-- | A register that holds nothing. If all are taken, the one that takes the
-- fewest stores to empty is emptied.
allocate :: Gen Reg
allocate = do
  held <- gets registers
  taken <- gets pinned
  stored <- gets inMemory
  mustKeep <- asks needed
  let candidates = filter (`notElem` taken) allocatable
      contents reg = Map.findWithDefault Set.empty reg held
      cost reg = Set.size ((contents reg `Set.intersection` mustKeep) `Set.difference` stored)
  reg <- case (filter (Set.null . contents) candidates, candidates) of
    (free : _, _) -> return free
    ([], [])      -> internalError "a single quad uses every register"
    ([], _)       -> do
      let victim = minimumBy (comparing cost) candidates
      release victim mustKeep
      return victim
  reg <$ pin reg

-- | The register holding the value of an operand, which is loaded first if
-- it is not in a register yet.
inRegister :: Operand -> Gen Reg
inRegister operand = use operand >>= \case
  Register reg -> return reg
  source -> do
    reg <- allocate
    move (operandWidth operand) source (Register reg)
    case operand of
      Ref var -> modify $ \state ->
        state { registers = Map.insert reg (Set.singleton var) (registers state) }
      Const _ _ -> return ()
    return reg

-- | A register that holds the value of an operand and may be overwritten
-- with the value of the target variable. The register the operand already
-- lives in is reused when nothing else needs what it holds.
scratch :: Var -> Operand -> Gen Reg
scratch target operand = do
  taken <- gets pinned
  source <- use operand
  live <- asks liveNow
  held <- gets registers
  let contents reg = Map.findWithDefault Set.empty reg held
      unneeded var = var == target || var `Set.notMember` live
      expendable reg = reg `notElem` taken && all unneeded (contents reg)
  case source of
    Register reg | expendable reg -> return reg
    _ -> do
      reg <- allocate
      move (operandWidth operand) source (Register reg)
      return reg

stringLiteralSymbol :: String -> Gen String
stringLiteralSymbol text = do
  literals <- gets strings
  case Map.lookup text literals of
    Just index -> return (stringSymbol index)
    Nothing -> do
      let index = Map.size literals
      modify $ \state -> state { strings = Map.insert text index literals }
      return (stringSymbol index)
