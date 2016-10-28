module WACC.Semantics.Typing where

import Data.Maybe

import           Control.Monad.State
import           WACC.Parser.Types
import           WACC.Parser.Primitives
import           WACC.Semantics.Types
import           WACC.Semantics.Primitives

unOpTypes
  = [ (Not, (TBool, TBool))
    , (Neg, (TInt, TInt))
    , (Len, (TArray TArb, TInt))
    , (Ord, (TChar, TInt))
    , (Chr, (TInt, TChar))
    ]

-- FIXME: investigate whether Add/Sub/Mul/Div/Mod/And/Or take TChar's too.
-- FIXME: Eq and NEq should work for other types
-- FIXME: Assign is one weird hack, also is return type good?
binAppTypes
  = [ (Assign, (TArb, TArb, TBool))
    , (Add, (TInt, TInt, TInt))
    , (Sub, (TInt, TInt, TInt))
    , (Mul, (TInt, TInt, TInt))
    , (Div, (TInt, TInt, TInt))
    , (Mod, (TInt, TInt, TInt))
    , (And, (TInt, TInt, TInt))
    , (Or, (TInt, TInt, TInt))
    , (Gt, (TInt, TInt, TBool))
    , (Gte, (TInt, TInt, TBool))
    , (Lt, (TInt, TInt, TBool))
    , (Lte, (TInt, TInt, TBool))
    , (Eq, (TInt, TInt, TBool))
    , (NEq, (TInt, TInt, TBool))
    ]

getLiteralType :: Literal -> SemanticChecker Type
getLiteralType (CHAR _)        = return TChar
getLiteralType (INT _)         = return TInt
getLiteralType (BOOL _)        = return TBool
getLiteralType (STR _)         = return TString
getLiteralType (ARRAY (e : _)) = getType e

deconstructArrayType :: Type -> SemanticChecker Type
deconstructArrayType (TArray t)
  = return t
deconstructArrayType t
  = invalid SemanticError $ "Expected TArray, Found: " ++ (show t)

getType :: Expr -> SemanticChecker Type
getType (Lit lit)
  = getLiteralType lit
getType (Ident id)
  = identLookup id
getType (ArrElem id _)
  = getType (Ident id) >>= deconstructArrayType
getType (PairElem e id) = do
  (TPair f s) <- getType (Ident id)
  return (pairElem e f s)
  where
    pairElem Fst f _ = f
    pairElem Snd _ s = s
getType (UnApp op _)
  = (maybe undefOp (return.fst)) . lookup op $ unOpTypes
getType (BinApp op _ _)
  = (maybe undefOp tripleFirst) . lookup op $ binAppTypes
  where
    tripleFirst (x,_,_) = return x
getType (FunCall id _)
  = identLookup id
getType (NewPair e1 e2)    -- FIXME: pair<T1,T2>
  = TPair <$> getType e1 <*> getType e2

addSymbol :: Symbol -> SemanticChecker ()
addSymbol s = do
  st <- gets symbolTable
  addToScope s st
 where
    notDefined (Symbol i _) symbs
      = all (\(Symbol ident _) -> i /= ident) symbs
    addToScope :: Symbol -> SymbolTable -> SemanticChecker ()
    addToScope s (SymbolTable ss []) = do
      locs <- gets locationData
      case notDefined s ss of
        True  -> put $ CheckerState locs (SymbolTable (s:ss) [])
        False -> invalid SemanticError "identifier already defined"
    addToScope s (SymbolTable ss [c]) = do
      locs <- gets locationData
      put $ CheckerState locs c
      addSymbol s
      newSt <- gets symbolTable
      put $ CheckerState locs newSt

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

identExists :: Identifier -> SemanticChecker ()
identExists i = do
  _ <- identLookup i
  return ()

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
