-- | The compiler frontend: everything between parsing and code generation.
--
-- The passes run in a fixed order that is visible to the programmer:
--
--   1. type checking, on the program exactly as written, so that code which
--      can never run, such as the branch of an @if (false)@, is still checked;
--   2. constant folding;
--   3. return checking, on the folded program, so that a function whose only
--      @return@ is under @if (true)@ is accepted.
module Latte.Frontend (analyse) where

import qualified Latte.Abs as Abs
import Latte.Core
import Latte.Error
import Latte.Frontend.ConstFold (foldStmts)
import Latte.Frontend.TypeCheck (typeCheck)
import Latte.Types

-- | Checks a program and translates it to the core language.
analyse :: Abs.Program -> Either CompileError Program
analyse program = do
  Program functions variables <- typeCheck program
  folded <- mapM (checkReturns . foldBody) functions
  return (Program folded variables)
  where
    foldBody function = function { functionBody = foldStmts (functionBody function) }

-- | Checks that a function that has to return a value cannot reach its end.
checkReturns :: Function -> Either CompileError Function
checkReturns function
  | functionReturn function == TVoid || any endsControl (functionBody function) = Right function
  | otherwise = Left $ CompileError (functionPosition function) $
      "function `" ++ functionName function ++ "` can finish without returning a value"

-- | Whether control never continues with the statement that follows.
--
-- Latte has no @break@, so a loop whose condition is the constant @true@ never
-- finishes. A call of the built-in @error@ ends the program.
endsControl :: Stmt -> Bool
endsControl stmt = case stmt of
  Return _                            -> True
  If _ whenTrue whenFalse             -> any endsControl whenTrue && any endsControl whenFalse
  While (BoolLit True) _              -> True
  Evaluate (Call (Builtin Error) _ _) -> True
  _                                   -> False
