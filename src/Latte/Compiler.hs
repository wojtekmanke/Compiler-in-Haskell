-- | The compiler as a pure function from source text to assembly.
--
-- > source --parse--> syntax tree --analyse--> core program
-- >        --generate--> quadruples --generateAssembly--> assembly
module Latte.Compiler (compile) where

import Latte.Backend.X86_64 (generateAssembly)
import Latte.Error
import Latte.Frontend (analyse)
import Latte.Par (myLexer, pProgram)
import Latte.Quadruples.Generate (generate)

-- | Compiles a Latte program to x86-64 assembly for GNU as.
compile :: String -> Either CompileError String
compile source = do
  program <- either (Left . syntaxError) Right (pProgram (myLexer source))
  core <- analyse program
  return (generateAssembly (generate core))

-- | Turns a message of the generated parser, such as
-- @syntax error at line 3, column 7 before \`)\'@, into an error with a
-- position. A message of any other shape is passed on unchanged.
syntaxError :: String -> CompileError
syntaxError message = case words message of
  "syntax" : "error" : "at" : "line" : line : "column" : column : rest
    | [(l, ",")] <- reads line, [(c, "")] <- reads column ->
        CompileError (Just (l, c)) (unwords ("syntax error" : rest))
  _ -> CompileError Nothing message
