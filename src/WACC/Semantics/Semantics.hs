module WACC.Semantics.Semantics where

import      WACC.Semantics.Types
import      WACC.Parser.Types

decreaseScope :: [SymbolTable] -> [SymbolTable]
decreaseScope []
  = []
decreaseScope (SymbolTable ((Symbol _ _) : st) : stt)
  = stt
decreaseScope ((SymbolTable []) : stt)
  = stt
decreaseScope ((SymbolTable st) : stt)
  = ((SymbolTable (decreaseScope st)) : stt)
decreaseScope st
  = st

increaseScope :: [SymbolTable] -> [SymbolTable]
increaseScope []
  = [SymbolTable []]
increaseScope ((Symbol i t) : stt)
  = [SymbolTable [], Symbol i t] ++ stt
increaseScope ((SymbolTable st) : stt)
  = (SymbolTable (increaseScope st)) : stt

addSymbol :: SymbolTable -> [SymbolTable] -> [SymbolTable]
addSymbol s ((SymbolTable st) : stt)
  = ((SymbolTable (addSymbol s st)) : stt)
addSymbol s st
  = (s : st)

isDefined :: SymbolTable -> [SymbolTable] -> Bool
isDefined s st
  = s `elem` (concatMap flatten st)

flatten :: SymbolTable -> [SymbolTable]
flatten (SymbolTable st)
  = concatMap flatten st
flatten s
  = [s]

semanticCheck :: Program -> SemanticChecker ()
semanticCheck p
  = return ()
