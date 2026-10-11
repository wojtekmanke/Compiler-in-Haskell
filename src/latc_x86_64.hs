import LexLatte
import ParLatte
import AbsLatte
import ErrM
import Frontend
import GenerateAsm

import System.Environment (getArgs)
import System.IO
import System.FilePath.Posix

import Data.Char

import qualified Data.Map.Lazy as Map

type ParseFun a = [Token] -> Err a

getTree :: ParseFun a -> String -> Either String a
getTree p s = let ts = myLexer s in case p ts of
  Bad s    -> Left s
  Ok  tree -> Right tree

main :: IO ()
main = do
  args <- getArgs
  s <- readInputProgram args
  let p = getTree pProgram s
  case p of
    Left err -> hPutStrLn stderr $ "ERROR\n" ++ err
    Right tree -> do
      (err, _) <- runFrontend $ analyzeProgram tree
      case err of
        Left errMsg -> hPutStrLn stderr $ "ERROR\n" ++ errMsg
        Right (classes, optimizedFunctions) -> do
          (err, _) <- runBackend $ generateCode classes optimizedFunctions
          case err of
            Left errMsg -> hPutStrLn stderr $ "ERROR\n" ++ errMsg
            Right _ -> hPutStrLn stderr $ "OK\n"
          return ()

-- Get program content
getFileName :: [String] -> String
getFileName (x:xs) = x
getFileName _ = ""

readInputProgram :: [String] -> IO String
readInputProgram [f] = readFile f
readInputProgram _ = error "Give exactly one argument"

capitalize :: String -> String
capitalize (c:cs) = ((toUpper c):cs)
capitalize [] = []
