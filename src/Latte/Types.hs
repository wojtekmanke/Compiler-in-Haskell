-- | The types of Latte values and functions, shared by all compiler passes.
module Latte.Types
  ( Type (..)
  , renderType
  , FunType (..)
  , Builtin (..)
  , builtins
  , fromSyntax
  , identName
  ) where

import qualified Latte.Abs as Abs

data Type
  = TInt
  | TString
  | TBool
  | TVoid
  | TClass String
  | TArray Type
  deriving (Eq, Ord, Show)

-- | A type in Latte syntax, for use in error messages.
renderType :: Type -> String
renderType ty = case ty of
  TInt           -> "int"
  TString        -> "string"
  TBool          -> "boolean"
  TVoid          -> "void"
  TClass name    -> name
  TArray element -> renderType element ++ "[]"

data FunType = FunType
  { returnType :: Type
  , paramTypes :: [Type]
  }
  deriving (Eq, Show)

-- | The functions that are part of the language. They are implemented by the
-- runtime library.
data Builtin = PrintInt | PrintString | ReadInt | ReadString | Error
  deriving (Eq, Show)

-- | The built-in functions with their names and types.
builtins :: [(String, Builtin, FunType)]
builtins =
  [ ("printInt",    PrintInt,    FunType TVoid [TInt])
  , ("printString", PrintString, FunType TVoid [TString])
  , ("readInt",     ReadInt,     FunType TInt [])
  , ("readString",  ReadString,  FunType TString [])
  , ("error",       Error,       FunType TVoid [])
  ]

-- | Converts a type from the syntax tree, dropping its source position.
fromSyntax :: Abs.Type' a -> Type
fromSyntax syntax = case syntax of
  Abs.Int _         -> TInt
  Abs.Str _         -> TString
  Abs.Bool _        -> TBool
  Abs.Void _        -> TVoid
  Abs.ClassT _ name -> TClass (identName name)
  Abs.Array _ t     -> TArray (fromSyntax t)

identName :: Abs.Ident -> String
identName (Abs.Ident name) = name
