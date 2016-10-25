module WACC.Semantics where

import      WACC.Parser.Types

runSemanticCheck :: AnnotatedProgram -> Program
runSemanticCheck
  = semanticChecker
