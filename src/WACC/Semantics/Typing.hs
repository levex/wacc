module WACC.Semantics.Typing where

import Data.Maybe

import           Control.Monad.State
import           WACC.Parser.Types
import           WACC.Parser.Primitives
import           WACC.Semantics.Types
import           WACC.Semantics.Primitives

-- FIXME: investigate whether Add/Sub/Mul/Div/Mod/And/Or take TChar's too.
-- FIXME: Assign is one weird hack, also is return type good?
unopType :: UnOp -> Type
unopType Not = TBool
unopType Neg = TInt
unopType Len = TInt
unopType Ord = TInt
unopType Chr = TChar

binopType :: BinOp -> Type
binopType Assign = TBool
binopType Add    = TInt
binopType Sub    = TInt
binopType Mul    = TInt
binopType Div    = TInt
binopType Mod    = TInt
binopType And    = TBool
binopType Or     = TBool
binopType Gt     = TBool
binopType Gte    = TBool
binopType Lt     = TBool
binopType Lte    = TBool
binopType Eq     = TBool
binopType NEq    = TBool

checkUnopArgs :: UnOp -> Type -> SemanticChecker ()
checkUnopArgs Not TBool         = valid
checkUnopArgs Neg TInt          = valid
checkUnopArgs Len (TArray _)    = valid
checkUnopArgs Ord TChar         = valid
checkUnopArgs Chr TInt          = valid
checkUnopArgs _ _               = invalid SemanticError "type error"

checkBinopArgs :: BinOp -> Type -> Type -> SemanticChecker ()
checkBinopArgs (Assign) t1 t2  = equalTypes "type error" t1 t2
checkBinopArgs Add TInt TInt   = valid
checkBinopArgs Sub TInt TInt   = valid
checkBinopArgs Mul TInt TInt   = valid
checkBinopArgs Div TInt TInt   = valid
checkBinopArgs Mod TInt TInt   = valid
checkBinopArgs And TBool TBool = valid
checkBinopArgs Or TBool TBool  = valid
checkBinopArgs Gt TInt TInt    = valid
checkBinopArgs Gt TChar TChar  = valid
checkBinopArgs Gt _ _          = invalid SemanticError "type error"
checkBinopArgs Gte TInt TInt   = valid
checkBinopArgs Gte TChar TChar = valid
checkBinopArgs Gte _ _         = invalid SemanticError "type error"
checkBinopArgs Lt TInt TInt    = valid
checkBinopArgs Lt TChar TChar  = valid
checkBinopArgs Lt _ _          = invalid SemanticError "type error"
checkBinopArgs Lte TInt TInt   = valid
checkBinopArgs Lte TChar TChar = valid
checkBinopArgs Lte _ _         = invalid SemanticError "type error"
checkBinopArgs Eq t1 t2        = equalTypes "type error" t1 t2
checkBinopArgs NEq t1 t2       = equalTypes "type error" t1 t2

getLiteralType :: Literal -> SemanticChecker Type
getLiteralType (CHAR _)        = return TChar
getLiteralType (INT _)         = return TInt
getLiteralType (BOOL _)        = return TBool
getLiteralType (STR _)         = return TString
getLiteralType (ARRAY (e : _)) = TArray <$> getType e
getLiteralType (ARRAY [])      = return (TArray TArb)
getLiteralType NULL            = return (TPair TArb TArb)

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
  = return $ unopType op
getType (BinApp op _ _)
  = return $ binopType op
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
equalTypes s (TArray t1) (TArray t2) = do
  t1 <- deconstructArrayType (TArray t1)
  t2 <- deconstructArrayType (TArray t2)
  equalTypes s t1 t2
equalTypes _ (TString) (TArray TChar) = valid
equalTypes _ (TArray TChar) (TString) = valid
equalTypes errMsg (TPair t11 t12) (TPair t21 t22)
  = equalTypes errMsg t11 t21 >> equalTypes errMsg t12 t22
equalTypes errMsg t1 t2
  | t1 == TArb || t2 == TArb = valid
  | t1 == t2                 = valid
  | otherwise                = invalid SemanticError errMsg
