module WACC.Semantics.Primitives where

import qualified Data.Map as Map
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           WACC.Semantics.Types
import           WACC.Parser.Types

valid :: SemanticChecker ()
valid
  = return ()

invalid :: ErrorType -> String -> SemanticChecker a
invalid e s
  = throwError $ CheckerError e (Location 0 0) s

rethrowWithLocation :: StatementId -> CheckerError -> SemanticChecker a
rethrowWithLocation i (CheckerError e _ s) = do
  ld <- gets locationData
  let loc = fromJust $ Map.lookup i (locations ld)
  throwError $ CheckerError e loc s