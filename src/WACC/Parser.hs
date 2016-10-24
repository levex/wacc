module WACC.Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import           WACC.Parser.Parser
import           WACC.Parser.Types

initialParserState :: UState
initialParserState = []

runWACCParser :: UState -> String -> Either ParseError Program
runWACCParser u
  = runParser program u "<cmdline>"
