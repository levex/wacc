module WACC.Semantics.Types where

import          WACC.Parser.Types

type Row = Int
type Column = Int

data SymbolTable
  = Symbol Identifier Type
  | SymbolTable [SymbolTable]
  deriving (Eq, Show)

data SemanticError = SemanticError Position String

data Position = Position Row Column
