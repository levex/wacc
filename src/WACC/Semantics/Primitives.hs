module WACC.Semantics.Primitives where

import          Control.Monad
import          Control.Monad.Except
import          WACC.Semantics.Types
import          WACC.Parser.Types

valid :: SemanticChecker ()
valid
  = return ()

invalid :: ErrorType -> String -> SemanticChecker a
invalid e s
  = throwError $ CheckerError e (Location 0 0) s
