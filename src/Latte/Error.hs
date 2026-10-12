-- | Errors reported to the user of the compiler.
module Latte.Error
  ( Position
  , CompileError (..)
  , renderError
  , internalError
  ) where

import Latte.Abs (BNFC'Position)

-- | Line and column of a construct in the source file, when known.
type Position = BNFC'Position

-- | A problem in the program being compiled.
data CompileError = CompileError
  { errorPosition :: Position
  , errorMessage  :: String
  }
  deriving (Eq, Show)

-- | Formats an error the way compilers conventionally do:
-- @file:line:column: error: message@.
renderError :: FilePath -> CompileError -> String
renderError file (CompileError position message) =
  file ++ location ++ ": error: " ++ message
  where
    location = case position of
      Just (line, column) -> ":" ++ show line ++ ":" ++ show column
      Nothing             -> ""

-- | Aborts on a broken invariant of the compiler itself. Reaching this is a
-- bug in the compiler and never the fault of the program being compiled.
internalError :: String -> a
internalError message = error ("internal compiler error: " ++ message)
