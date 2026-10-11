module Misc where

import AbsLatte

import qualified Data.Map.Lazy as Map

type PositionInfo = Maybe (Int, Int)
type StructMap = Map.Map String Struct
data T = Integer | String | Boolean | V | Class Ident | A T | Fun T [T]
  deriving (Show, Eq, Ord)
type Struct = Map.Map String T -- map from structure element to it's type

getIdentString :: Ident -> String
getIdentString (Ident s) = s

getClassName :: ClassDef a -> String
getClassName (ClDef _ name _) = getIdentString name

getClassElementName :: ClassEl a -> String
getClassElementName (ClElem _ _ ident) = getIdentString ident

getFunctionName :: TopDef a -> String
getFunctionName (FnDef _ _ name _ _) = getIdentString name

getArgName :: Arg a -> String
getArgName (Arg _ _ name) = getIdentString name

getItemName :: Item a -> String
getItemName (NoInit _ name) = getIdentString name
getItemName (Init _ name _) = getIdentString name

-- CONSTS
ptrSize :: Integer
ptrSize = 8
intSize :: Integer
intSize = 4
