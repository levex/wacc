module Main where

import           System.Environment
import           System.Exit

import           WACC.Parser
import           WACC.Semantics

main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' [file] = do
      contents <- readFile file
      case runWACCParser file contents of
        Left err -> (putStrLn $ show err) >> exitFailure
        Right p  -> case checkProgram p of
          Left err -> (putStrLn $ show err) >> exitFailure
          Right p  -> (putStrLn $ show p) >> exitSuccess
    main' _ = putStrLn "Usage: ./wacc <filename>"
