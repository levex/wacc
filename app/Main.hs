module Main where

import           System.Environment
import           System.Exit

import           WACC.Parser

main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' [file] = do
      contents <- readFile file
      case runParser file contents of
        Left err -> (putStrLn $ show err) >> exitFailure
        Right ast -> (putStrLn $ show ast) >> exitSuccess
    main' _ = putStrLn "Usage: ./wacc <filename>"
