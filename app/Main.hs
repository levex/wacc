module Main where

import           System.Environment
import           Parser

main :: IO ()
main = do
  args <- getArgs
  main' args

  where
    main' [file] = do
      contents <- readFile file
      case runParser file contents of
        Left err -> putStrLn $ show err
        Right ast -> putStrLn $ show ast
    main' _ = putStrLn "Usage: ./hcc <filename>"
