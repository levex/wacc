module WACC.Semantics where

import           Control.Monad.Except
import           Control.Monad.State
import           WACC.Parser.Types
import           WACC.Semantics.Types
import           WACC.Semantics.Syntax
import           WACC.Semantics.Semantics

checkProgram :: AnnotatedProgram -> Either CheckerError Program
checkProgram (p, ld)
  = (evalState . runExceptT . runSemanticChecker) (runCheck p) initialState
  where
    runCheck p = do
      syntaxCheck p
      semanticCheck p
      return p

    initialState = (CheckerState ld (SymbolTable []))
