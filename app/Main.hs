module Main where

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
  (file, options) <- processArgs [] <$> getArgs
  main' file options

  where
    main' "" _ = putStrLn "Usage: ./wacc [-I include_path] <filename>"
    main' file options = do
      let opts = [o ++ v | (o, v) <- options]
      contents <- readProcess "cpp" (opts ++ [file]) ""
      --contents <- readFile file
      --putStrLn "Input file: "
      --putStrLn ""
      --putStrLn $ concat $ zipWith (\str c -> show c ++ "\t" ++ str ++ "\n") (lines contents) [1..]
      --putStrLn "\n----------------------------\n"
      case runWACCParser file contents of
        Left err -> (putStrLn $ show err) >> exitWith (ExitFailure 100)
        Right p  -> case checkProgram p of
          Left err -> (putStrLn $ show err) >> exitWith (compilationError err)
          Right p  -> do
            --putStrLn $ "AST: "
            --putStrLn ""
            --print p
            --putStrLn "\n----------------------------\n"
            let code = generateCode (optimizeProgram p)
            --putStrLn code
            writeFile (replaceExtension file ".s") code
            exitSuccess

    processArgs options ("-I":includePath:args)
      = processArgs (("-I", includePath):options) args
    processArgs options (_:_:args)
      = processArgs options args
    processArgs options [f]
      = (f, options)
    processArgs options []
      = ("", options)

    compilationError err
      = ExitFailure (getExitCode err 100 200 200)
