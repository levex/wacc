module WACC.Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import           WACC.Parser.Parser
import           WACC.Parser.Types

initialParserState :: UState
initialParserState = []

runWACCParser :: UState -> String -> Either ParseError Program
runWACCParser u s
  = runParser program u "<cmdline>" s
