{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WACC.Optimizations.Types where

import           Control.Monad.State
import           WACC.Parser.Types

data OptimizerState = OptimizerState

newtype Optimizer a = Optimizer
  { runOptimizer :: State OptimizerState a }
      deriving (Functor, Applicative, Monad,
                MonadState OptimizerState)

