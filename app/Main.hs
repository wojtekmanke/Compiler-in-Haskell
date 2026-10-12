-- | The command-line interface of the compiler.
module Main (main) where

import Control.Exception (IOException, evaluate, try)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO

import Latte.Compiler (compile)
import Latte.Error (renderError)

main :: IO ()
main = do
  -- File names are echoed in messages. This encoding writes any of them back
  -- byte for byte, whatever the locale made of them on the way in.
  mkTextEncoding "UTF-8//ROUNDTRIP" >>= hSetEncoding stderr
  args <- getArgs
  case args of
    [input]               -> run input Nothing
    [input, "-o", output] -> run input (Just output)
    ["-o", output, input] -> run input (Just output)
    _ -> do
      name <- getProgName
      failWith ("usage: " ++ name ++ " FILE.lat [-o FILE.s]")

-- | Compiles a file and writes the assembly to the given file, or to the
-- standard output.
run :: FilePath -> Maybe FilePath -> IO ()
run input output = do
  source <- orFail $ withFile input ReadMode $ \handle -> do
    hSetEncoding handle utf8  -- source files are in UTF-8
    hGetContents' handle
  case compile source of
    Left problem -> failWith (renderError input problem)
    Right assembly -> do
      -- Nothing is written unless the whole program compiles.
      _ <- evaluate (length assembly)
      orFail $ case output of
        Nothing   -> write stdout assembly
        Just path -> withFile path WriteMode (`write` assembly)
  where
    write handle text = hSetEncoding handle utf8 >> hPutStr handle text

-- | Runs an action, reporting a failure to read or write a file.
orFail :: IO a -> IO a
orFail action = do
  result <- try action
  case result of
    Left problem -> failWith ("error: " ++ show (problem :: IOException))
    Right value  -> return value

failWith :: String -> IO a
failWith message = hPutStrLn stderr message >> exitFailure
