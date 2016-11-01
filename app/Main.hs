module Main where

import           System.Environment
import           System.Exit

import           WACC.Parser
import           WACC.Semantics
import           WACC.CodeGen

-- FIXME: this needs rewriting to use >>=
main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' [file] = do
      contents <- readFile file
      case runWACCParser file contents of
        Left err -> (putStrLn $ show err) >> exitWith (ExitFailure 100)
        Right p  -> case checkProgram p of
          Left err -> (putStrLn $ show err) >> exitWith (compilationError err)
          Right p  -> do
            print p
            putStrLn ""
            putStrLn ""
            putStrLn $ generateCode p
            exitSuccess
    main' _ = putStrLn "Usage: ./wacc <filename>"

    compilationError err
      = ExitFailure (getExitCode err 100 200 200)
