module WACC.Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import           WACC.Parser.Parser
import           WACC.Parser.Types

runWACCParser :: String -> String -> Either ParseError Program
runWACCParser
  = runParser program initialParserState
