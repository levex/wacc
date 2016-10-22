module WACC.Parser where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Error
import           WACC.Parser.Parser
import           WACC.Parser.Types

runParser :: String -> String -> Either ParseError Program
runParser
  = parse program
