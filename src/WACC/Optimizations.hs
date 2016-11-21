module WACC.Optimizations where

import           Control.Monad.State
import           WACC.Parser.Types
import           WACC.Optimizations.Types
import           WACC.Optimizations.Expressions

optimizeProgram :: Program -> Program
optimizeProgram p = (flip evalState OptimizerState . runOptimizer) $ do
  minimizeExpressions p

