-- | The representation of Latte values in memory.
--
-- Integers and booleans take 32 bits. Strings, arrays and objects are
-- references and take 64:
--
--   * a string is a pointer to NUL-terminated bytes, and the null pointer
--     stands for the empty string;
--   * an array is a pointer to its 32-bit length, which is followed,
--     'arrayHeader' bytes in, by its elements;
--   * an object is a pointer to its fields.
--
-- The runtime library relies on the same representation.
module Latte.Layout
  ( Width (..)
  , widthOf
  , bytes
  , Field (..)
  , Layout (..)
  , layout
  , arrayHeader
  ) where

import qualified Data.Map as Map

import Latte.Types

-- | The size of a value.
data Width = W32 | W64
  deriving (Eq, Ord, Show)

widthOf :: Type -> Width
widthOf ty = case ty of
  TInt     -> W32
  TBool    -> W32
  TVoid    -> W32  -- has no values; only ever asked for the unused result of a call
  TString  -> W64
  TClass _ -> W64
  TArray _ -> W64

bytes :: Width -> Int
bytes W32 = 4
bytes W64 = 8

-- | Where a field is inside an object, and what it holds.
data Field = Field
  { fieldOffset :: Int
  , fieldType   :: Type
  }
  deriving (Eq, Show)

-- | The arrangement of the fields of a class.
data Layout = Layout
  { layoutSize   :: Int
  , layoutFields :: Map.Map String Field
  }

-- | Lays fields out in declaration order, each aligned to its own size.
layout :: [(String, Type)] -> Layout
layout fields = Layout (alignTo 8 end) (Map.fromList placed)
  where
    (end, placed) = foldl place (0, []) fields
    place (offset, done) (name, ty) =
      let size  = bytes (widthOf ty)
          start = alignTo size offset
      in (start + size, (name, Field start ty) : done)
    alignTo size offset = (offset + size - 1) `div` size * size

-- | The distance from the start of an array to its first element.
arrayHeader :: Int
arrayHeader = 8
