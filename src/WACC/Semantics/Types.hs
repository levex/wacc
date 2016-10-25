module WACC.Semantics.Types where

import          WACC.Parser.Types

data SymbolTable
  = Symbol Identifier Type
  | SymbolTable [SymbolTable]
  deriving (Eq, Show)

