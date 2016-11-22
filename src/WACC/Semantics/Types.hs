{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WACC.Semantics.Types where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map as Map
import           Data.Map (Map)
import           WACC.Parser.Types

data CheckerState = CheckerState
  { locationData :: LocationData,
    symbolTable :: SymbolTable,
    structDefs :: [(Identifier, Map.Map Identifier (Offset, Type))] }

data ErrorType
  = SyntaxError
  | SemanticError
  | TypeError
  deriving (Eq)

instance Show ErrorType where
  show SyntaxError    = "Syntax Error"
  show SemanticError  = "Semantic Error"
  show TypeError      = "Type Error"

data CheckerError
  = CheckerError ErrorType Location String
  deriving (Eq)

instance Show CheckerError where
  show (CheckerError e loc s)
    = show e ++ " in statement on line " ++ show (row loc)
      ++ ", column " ++ show (column loc) ++ detail
      where
        detail
          | null s    = ""
          | otherwise = ": " ++ s

newtype SemanticChecker a = SemanticChecker
  { runSemanticChecker :: ExceptT CheckerError (State CheckerState) a }
      deriving (Functor, Applicative, Monad,
                MonadState CheckerState,
                MonadError CheckerError)

data Symbol = Symbol
  { symbolIdent :: Identifier,
    symbolType :: Type}
  deriving (Eq, Show)

data SymbolTable
  = SymbolTable [Symbol] [SymbolTable]
  deriving (Eq, Show)
