module WACC.Optimizations where

import           Control.Monad.State
import           WACC.Parser.Types
import           WACC.Optimizations.Types
import           WACC.Optimizations.Expressions

optimize :: Program -> Program
optimize p
  = (evalState . runOptimizer) (optimizeProgram p) OptimizerState
  where
    optimizeProgram :: Program -> Optimizer Program
    optimizeProgram p = do
      minimizeExpressions p

