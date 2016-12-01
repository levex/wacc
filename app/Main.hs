module Main where

import           Data.List
import           Control.Monad

import           System.FilePath
import           System.Environment
import           System.Exit
import           System.Process

import           WACC.Parser
import           WACC.Semantics
import           WACC.Optimizations
import           WACC.CodeGen

-- FIXME: this needs rewriting to use >>=
main :: IO ()
main = do
  args <- getArgs
  let (options, [file]) = splitAt (length args - 1) args
  main' file options

  where
    main' "" _ = putStrLn "Usage: ./wacc [-Iinclude_path] [-q] <filename>"
    main' file options = do
      let includePaths = filter (isPrefixOf "-I") options
      contents <- readProcess "cpp" (includePaths ++ [file]) ""
      unless ("-q" `elem` options) $ do
        putStrLn "Input file: "
        putStrLn ""
        putStrLn $ concat $ zipWith (\str c -> show c ++ "\t" ++ str ++ "\n") (lines contents) [1..]
        putStrLn "\n----------------------------\n"
      case runWACCParser file contents of
        Left err -> (putStrLn $ show err) >> exitWith (ExitFailure 100)
        Right p  -> case checkProgram p of
          Left err -> (putStrLn $ show err) >> exitWith (compilationError err)
          Right p  -> do
            let code = generateCode (optimizeProgram p)
            unless ("-q" `elem` options) $ do
              putStrLn $ "AST: "
              putStrLn ""
              print p
              putStrLn "\n----------------------------\n"
              putStrLn code
            writeFile (replaceExtension file ".S") code
            exitSuccess

    compilationError err
      = ExitFailure (getExitCode err 100 200 200)
