module WACC.Semantics.Semantics where

import      WACC.Semantics.Types
import      WACC.Semantics.Primitives
import      WACC.Parser.Types
import      Control.Monad
import      Control.Monad.State
import      Control.Monad.Except
import      Data.Maybe

addSymbol :: Symbol -> SymbolTable -> SymbolTable
addSymbol s (SymbolTable ss [])
  = (SymbolTable (s:ss) [])
addSymbol s (SymbolTable ss [c])
  = (SymbolTable ss [addSymbol s c])

decreaseScope :: SymbolTable -> SymbolTable
decreaseScope (SymbolTable s [(SymbolTable ss [])])
  = SymbolTable s []
decreaseScope (SymbolTable s [c])
  = (SymbolTable s [decreaseScope c])
decreaseScope st
  = SymbolTable [] []

increaseScope :: SymbolTable -> SymbolTable
increaseScope (SymbolTable s [])
  = (SymbolTable s [(SymbolTable [] [])])
increaseScope (SymbolTable s [c])
  = (SymbolTable s [increaseScope c])

identLookup :: Identifier -> SemanticChecker Type
identLookup i = do
  st <- gets symbolTable
  case getTypeForId i st of
    (Just t) -> return t
    Nothing  -> invalid SemanticError "undefined identifier"

getTypeForId :: Identifier -> SymbolTable -> Maybe Type
getTypeForId i (SymbolTable s [])
  = lookup i (map (\(Symbol i t) -> (i, t)) s)
getTypeForId i (SymbolTable s [ch])
  | res /= Nothing = res
  | otherwise      = getTypeForId i (SymbolTable s [])
  where
    res = getTypeForId i ch

semanticCheck :: Program -> SemanticChecker ()
semanticCheck p
  = return ()
