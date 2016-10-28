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
    , (And, (TBool, TBool, TBool))
    , (Or, (TBool, TBool, TBool))
    , (Gt, (TInt, TInt, TBool))
    , (Gte, (TInt, TInt, TBool))
    , (Lt, (TInt, TInt, TBool))
    , (Lte, (TInt, TInt, TBool))
    , (Eq, (TArb, TArb, TBool))
    , (NEq, (TArb, TArb, TBool))
    ]

getLiteralType :: Literal -> SemanticChecker Type
getLiteralType (CHAR _)        = return TChar
getLiteralType (INT _)         = return TInt
getLiteralType (BOOL _)        = return TBool
getLiteralType (STR _)         = return TString
getLiteralType (ARRAY (e : _)) = TArray <$> getType e

deconstructArrayType :: Type -> SemanticChecker Type
deconstructArrayType (TArray t)
  = return t
deconstructArrayType (TString)
  = return TChar
deconstructArrayType t
  = invalid SemanticError $ "Expected TArray, Found: " ++ show t

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
  = maybe undefOp (return.snd) . lookup op $ unOpTypes
getType (BinApp op _ _)
  = maybe undefOp tripleFirst . lookup op $ binAppTypes
  where
    tripleFirst (_,_,x) = return x
getType (FunCall id _) =
  identLookup id >>= getRetType
  where
    getRetType (TFun rT _) = return rT
    getRetType _ = invalid SemanticError "identifier is not a function"
getType (NewPair e1 e2)    -- FIXME: pair<T1,T2>
  = TPair <$> getType e1 <*> getType e2

addSymbol :: Symbol -> SemanticChecker ()
addSymbol s = do
  st <- gets symbolTable
  addToScope s st
 where
    notDefined (Symbol i _)
      = all (\(Symbol ident _) -> i /= ident)
    addToScope :: Symbol -> SymbolTable -> SemanticChecker ()
    addToScope s (SymbolTable ss []) = do
      locs <- gets locationData
      if notDefined s ss then
        put $ CheckerState locs (SymbolTable (s:ss) [])
      else
        invalid SemanticError "identifier already defined"
    addToScope s (SymbolTable ss [c]) = do
      locs <- gets locationData
      put $ CheckerState locs c
      addSymbol s
      newSt <- gets symbolTable
      put $ CheckerState locs (SymbolTable ss [newSt])

decreaseScope :: SemanticChecker ()
decreaseScope = do
  st <- gets symbolTable
  locs <- gets locationData
  put (CheckerState locs (decreaseScope' st))
  where
    decreaseScope' (SymbolTable s [SymbolTable ss []])
      = SymbolTable s []
    decreaseScope' (SymbolTable s [c])
      = SymbolTable s [decreaseScope' c]
    decreaseScope' st
      = SymbolTable [] []

increaseScope :: SemanticChecker ()
increaseScope = do
  st <- gets symbolTable
  locs <- gets locationData
  put (CheckerState locs (increaseScope' st))
  where
    increaseScope' (SymbolTable s [])
      = SymbolTable s [SymbolTable [] []]
    increaseScope' (SymbolTable s [c])
      = SymbolTable s [increaseScope' c]

scoped :: SemanticChecker () -> SemanticChecker ()
scoped stmt
  = increaseScope *> stmt <* decreaseScope

identExists :: Identifier -> SemanticChecker ()
identExists i = do
  _ <- identLookup i
  return ()

identLookup :: Identifier -> SemanticChecker Type
identLookup i = do
  st <- gets symbolTable
  case getTypeForId i st of
    (Just t) -> return t
    Nothing  -> invalid SemanticError $ "undefined identifier " ++ show(i)

getTypeForId :: Identifier -> SymbolTable -> Maybe Type
getTypeForId i (SymbolTable s [])
  = lookup i (map (\(Symbol i t) -> (i, t)) s)
getTypeForId i (SymbolTable s [ch])
  | isJust res = res
  | otherwise  = getTypeForId i (SymbolTable s [])
  where
    res = getTypeForId i ch

equalTypes :: String -> Type -> Type -> SemanticChecker ()
equalTypes _ (TString) (TArray TChar) = valid
equalTypes _ (TArray TChar) (TString) = valid
equalTypes errMsg t1 t2
  | t1 == TArb || t2 == TArb = valid
  | t1 == t2                 = valid
  | otherwise                = invalid SemanticError errMsg
