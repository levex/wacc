module WACC.Semantics.Syntax where

import           WACC.Parser.Types
import           WACC.Semantics.Types

syntaxCheck :: Program -> SemanticChecker ()
syntaxCheck program
  = return ()
