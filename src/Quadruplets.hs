module Quadruplets where

import Misc
import AbsLatte
import qualified Data.Set as Set

data SimpleBlock = SimpleBlock String [Quad] [Var]
  deriving Show

data QType = QString | QVal | QClass String | QArrayRef QType
  deriving Show

data Var = Var QType String | IConst Integer | Null | QTrue | QFalse
  deriving Show

data QuadWithLive = QL Quad (Set.Set String) -- list of live variables after the operation
  deriving Show

data Quad
  = VReturn -- void return
  | ConstInt Var Integer -- var := value
  | ConstString Var String -- var := value
  | StoreStruct Var String Var -- struct.filed = val
  | StoreArray Var Var Var -- a[index] = val
  | Assign Var Var -- var1 := var2
  | Malloc Var Var -- var := malloc(number of bytes)
  | ArrayMalloc Var Var Var -- var := malloc(number of bytes) actual size
  | BinOp Var Var BinOp Var -- var1 = var2 op var3
  | Call String Var [Var] -- fName, result, params
  | Error -- error function
  | PrintString Var -- print string
  | PrintInt Var -- print integer
  | ReadInt Var -- read integer and save in var
  | ReadStr Var -- read string and save in var
  | ClassElem Var Var String -- var1 := var2.field
  | ArrayElem Var Var Var -- var1 := var2[var3]
  | Negate Var Var -- var1 := -var2
  | Concat Var Var Var -- var1 := var2+var3
  | Jump String -- Unconditional jump
  | CTJump Var String -- Conditional jump if var is true
  | CFJump Var String -- conditional jump if var is false
  | LTJump Var Var String -- jump if v1 < v2
  | LEJump Var Var String -- jump if v1 <= v2
  | GTJump Var Var String -- jump if v1 > v2
  | GEJump Var Var String -- jump if v1 >= v2
  | EQJump Var Var String -- jump if v1 == v2
  | NEQJump Var Var String -- jump if v1 != v2
  | QReturn Var -- return val
  | ArrayLen Var Var -- t = a.length
  | Increment Var -- increment value
  | Decrement Var -- decrement value
  | IncrementField Var String -- struct.filed++
  | DecrementField Var String -- struct.field--
  | IncrementArr Var Var -- a[i]++
  | DecrementArr Var Var -- a[i]--
  deriving Show

data BinOp
  = QPlus | QMinus | QTimes | QDiv | QMod | QLTH | QLE | QGTH | QGE | QEQU | QNE
  deriving (Show, Eq)

getType :: Type a -> QType
getType (Int _) = QVal
getType (Str _) = QString
getType (Bool _) = QVal
getType (Void _) = QVal
getType (ClassT _ ident) = QClass (getIdentString ident)
getType (Array _ t) = QArrayRef (getType t)

getType2 :: T -> QType
getType2 (Integer) = QVal
getType2 (String) = QString
getType2 (Boolean) = QVal
getType2 (V) = QVal
getType2 (Class ident) = QClass (getIdentString ident)
getType2 (A t) = QArrayRef (getType2 t)


--data RelOp
--  = OpLTH | OpLE | GTH | GE | EQU | NE
--  deriving Show
