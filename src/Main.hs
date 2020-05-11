module Main where

import System.Environment ( getArgs )

import Messages.Messages ( printNoFile, printTooManyFiles )
import Start.Start ( runFile )

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> printNoFile
    [x] -> runFile x 
    _ -> printTooManyFiles
