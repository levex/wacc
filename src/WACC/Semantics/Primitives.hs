module WACC.Semantics.Primitives where

import          Control.Monad
import          Control.Monad.Except
import          WACC.Semantics.Types
import          WACC.Parser.Types

valid :: SemanticChecker ()
valid
  = return ()

invalid :: String -> SemanticChecker ()
invalid s
  = throwError $ CheckerError SyntaxError (Location 0 0) s

semanticInvalid :: String -> SemanticChecker Type
semanticInvalid s
  = throwError $ CheckerError SemanticError (Location 0 0) s
