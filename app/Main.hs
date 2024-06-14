module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)

todo s = error ("todo: " ++ s)

main = do
  args <- getArgs
  case args of
    [path] -> do
      src <- readFile path
      tokens <- either die return (tokenize src)
      program <-
        either die return (runParser parseProgram $ parserSource tokens)
      writeFile "out.asm" (genAsm program)
    _ -> die "incorrect arguments set\n  usage: bpl path/to/file"

die err = do
  putStrLn err
  exitFailure
