{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WACC.Semantics.Types where

import           Control.Monad.Except
import           Control.Monad.State
import           WACC.Parser.Types

data CheckerState = CheckerState
  { locationData :: LocationData }

data ErrorType
  = SyntaxError
  | SemanticError
  | TypeError
  deriving (Eq, Show)

data CheckerError
  = CheckerError ErrorType Location
  deriving (Eq, Show)

newtype SemanticChecker a = SemanticChecker
  { runSemanticChecker :: ExceptT CheckerError (State CheckerState) a }
      deriving (Functor, Applicative, Monad,
                MonadState CheckerState,
                MonadError CheckerError)

data SymbolTable
  = Symbol Identifier Type
  | SymbolTable [SymbolTable]
  deriving (Eq, Show)

