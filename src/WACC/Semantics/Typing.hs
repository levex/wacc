{-# LANGUAGE RecordWildCards #-}
module WACC.Semantics.Typing where

import Data.Maybe

import           Control.Monad.State
import           WACC.Parser.Types
import qualified Data.Map as Map
import           Data.Map (Map)
import           WACC.Parser.Primitives
import           WACC.Semantics.Types
import           WACC.Semantics.Primitives

-- FIXME: investigate whether Add/Sub/Mul/Div/Mod/And/Or take TChar's too.
-- FIXME: Assign is one weird hack, also is return type good?
unopType :: UnOp -> Expr -> SemanticChecker Type
unopType Not e    = pure TBool
unopType Neg e    = pure TInt
unopType Len e    = pure TInt
unopType Ord e    = pure TInt
unopType Chr e    = pure TChar
unopType PreInc e   = getType e
unopType PostInc e  = getType e
unopType PreDec e   = getType e
unopType PostDec e  = getType e
unopType Deref e    = getType e
unopType AddrOf e   = TPtr <$> getType e
unopType BwNot e    = pure TInt

binopType :: BinOp -> Expr -> Expr -> SemanticChecker Type
binopType Add    e1 e2 = getType e1
binopType Sub    e1 e2 = getType e1
binopType Mul    e1 e2 = pure TInt
binopType Div    e1 e2 = pure TInt
binopType Mod    e1 e2 = pure TInt
binopType And    e1 e2 = pure TBool
binopType Or     e1 e2 = pure TBool
binopType Gt     e1 e2 = pure TBool
binopType Gte    e1 e2 = pure TBool
binopType Lt     e1 e2 = pure TBool
binopType Lte    e1 e2 = pure TBool
binopType Eq     e1 e2 = pure TBool
binopType NEq    e1 e2 = pure TBool
binopType Member e1 e2 = getStructType e1 e2
binopType BwAnd  e1 e2 = pure TInt
binopType BwOr   e1 e2 = pure TInt
binopType BwXor  e1 e2 = pure TInt
binopType BwShiftL e1 e2  = pure TInt
binopType BwShiftR e1 e2  = pure TInt
binopType Assign          e1 e2 = getType e1
binopType AddAssign       e1 e2 = getType e1
binopType SubAssign       e1 e2 = getType e1
binopType MulAssign       e1 e2 = getType e1
binopType DivAssign       e1 e2 = getType e1
binopType ModAssign       e1 e2 = getType e1
binopType BwAndAssign     e1 e2 = getType e1
binopType BwOrAssign      e1 e2 = getType e1
binopType BwXorAssign     e1 e2 = getType e1
binopType BwShiftLAssign  e1 e2 = getType e1
binopType BwShiftRAssign  e1 e2 = getType e1

isIntType :: Type -> Bool
isIntType (TPtr _) = True
isIntType t = t `elem` [TChar, TInt, TUInt8, TUInt16, TUInt32]

validateType :: Bool -> SemanticChecker ()
validateType = validate SemanticError "type error"

checkUnopArgs :: UnOp -> Type -> SemanticChecker ()
checkUnopArgs Not TBool         = valid
checkUnopArgs Neg TInt          = valid
checkUnopArgs Len (TArray _)    = valid
checkUnopArgs Ord TChar         = valid
checkUnopArgs Chr TInt          = valid

checkUnopArgs PreInc  t = validateType $ isIntType t
checkUnopArgs PostInc t = validateType $ isIntType t
checkUnopArgs PreDec  t = validateType $ isIntType t
checkUnopArgs PostDec t = validateType $ isIntType t

checkUnopArgs Deref (TPtr _)    = valid
checkUnopArgs AddrOf _          = valid

checkUnopArgs BwNot t           = validateType $ isIntType t

checkUnopArgs _ _               = invalid SemanticError "type error"

checkBinopArgs :: BinOp -> Type -> Type -> SemanticChecker ()
checkBinopArgs Add t1 t2 = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Sub t1 t2 = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Mul t1 t2 = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Div t1 t2 = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Mod t1 t2 = validateType $ isIntType t1 && isIntType t2

checkBinopArgs And TBool TBool = valid
checkBinopArgs Or TBool TBool  = valid

checkBinopArgs Gt t1 t2        = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Gte t1 t2       = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Lt t1 t2        = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Lte t1 t2       = validateType $ isIntType t1 && isIntType t2
checkBinopArgs Eq t1 t2        = equalTypes "type error" t1 t2
checkBinopArgs NEq t1 t2       = equalTypes "type error" t1 t2

checkBinopArgs BwAnd t1 t2    = validateType $ isIntType t1 && isIntType t2
checkBinopArgs BwOr t1 t2     = validateType $ isIntType t1 && isIntType t2
checkBinopArgs BwXor t1 t2    = validateType $ isIntType t1 && isIntType t2
checkBinopArgs BwShiftL t1 t2 = validateType $ isIntType t1 && isIntType t2
checkBinopArgs BwShiftR t1 t2 = validateType $ isIntType t1 && isIntType t2

checkBinopArgs Assign t1 t2         = equalTypes "type error" t1 t2
checkBinopArgs AddAssign t1 t2      = equalTypes "type error" t1 t2
checkBinopArgs SubAssign t1 t2      = equalTypes "type error" t1 t2
checkBinopArgs MulAssign t1 t2      = equalTypes "type error" t1 t2
checkBinopArgs DivAssign t1 t2      = equalTypes "type error" t1 t2
checkBinopArgs ModAssign t1 t2      = equalTypes "type error" t1 t2
checkBinopArgs BwAndAssign t1 t2    = equalTypes "type error" t1 t2
checkBinopArgs BwOrAssign t1 t2     = equalTypes "type error" t1 t2
checkBinopArgs BwXorAssign t1 t2    = equalTypes "type error" t1 t2
checkBinopArgs BwShiftLAssign t1 t2 = equalTypes "type error" t1 t2
checkBinopArgs BwShiftRAssign t1 t2 = equalTypes "type error" t1 t2

checkBinopArgs Member _ _           = valid -- assume valid

checkBinopArgs _ _ _                = invalid SemanticError "type error"

getLiteralType :: Literal -> SemanticChecker Type
getLiteralType (CHAR _)        = return TChar
getLiteralType (INT _)         = return TInt
getLiteralType (BOOL _)        = return TBool
getLiteralType (STR _)         = return TString
getLiteralType (ARRAY (e : _)) = TArray <$> getType e
getLiteralType (ARRAY [])      = return (TArray TArb)
getLiteralType NULL            = return (TPtr TArb)

deconstructArrayType :: Type -> SemanticChecker Type
deconstructArrayType (TArray t)
  = return t
deconstructArrayType (TString)
  = return TChar
deconstructArrayType t
  = invalid SemanticError $ "Expected TArray, Found: " ++ show t

getStructType :: Expr -> Expr -> SemanticChecker Type
getStructType e1 (Ident m) = do
  (TPtr (TStruct i)) <- getType e1
  structs <- gets structDefs
  let members = fromJust $ Map.lookup i structs
  case lookup m members of
    Nothing -> invalid SemanticError $ "Struct " ++ show i ++
      " does not contain member " ++ show m
    Just t -> return t
getStructType _ _
  = invalid SemanticError $ "Invalid expression for accessing struct member"

getType :: Expr -> SemanticChecker Type
getType (Lit lit)
  = getLiteralType lit
getType (Ident id)
  = identLookup id
getType (ArrElem id es)
  = getType (Ident id) >>= foldr1 (>=>) (map (const deconstructArrayType) es)
getType (PairElem e id) = do
  (TPair f s) <- getType (Ident id)
  return (pairElem e f s)
  where
    pairElem Fst f _ = f
    pairElem Snd _ s = s
getType (UnApp op e)
  = unopType op e
getType (BinApp op e1 e2)
  = binopType op e1 e2
getType (FunCall id _) =
  identLookup id >>= getRetType
  where
    getRetType (TFun rT _) = return rT
    getRetType _ = invalid SemanticError "identifier is not a function"
getType (NewPair e1 e2)    -- FIXME: pair<T1,T2>
  = TPair <$> getType e1 <*> getType e2
getType (SizeOf _)
  = return TInt
getType (OffsetOf _ _)
  = return TInt

addSymbol :: Symbol -> SemanticChecker ()
addSymbol s = do
  st <- gets symbolTable
  addToScope s st
 where
    notDefined (Symbol i _)
      = all (\(Symbol ident _) -> i /= ident)
    addToScope :: Symbol -> SymbolTable -> SemanticChecker ()
    addToScope s (SymbolTable ss []) = do
      st@CheckerState{..} <- get
      if notDefined s ss then
        put st{symbolTable = (SymbolTable (s:ss) [])}
      else
        invalid SemanticError "identifier already defined"
    addToScope s (SymbolTable ss [c]) = do
      locs <- gets locationData
      structs <- gets structDefs
      put $ CheckerState locs c structs
      addSymbol s
      newSt <- gets symbolTable
      put $ CheckerState locs (SymbolTable ss [newSt]) structs

storeDecl :: Declaration -> SemanticChecker ()
storeDecl (ident, t)
  = addSymbol (Symbol ident t)

decreaseScope :: SemanticChecker ()
decreaseScope = do
  s@CheckerState{..} <- get
  put s{symbolTable = decreaseScope' symbolTable}
  where
    decreaseScope' (SymbolTable s [SymbolTable ss []])
      = SymbolTable s []
    decreaseScope' (SymbolTable s [c])
      = SymbolTable s [decreaseScope' c]
    decreaseScope' st
      = SymbolTable [] []

increaseScope :: SemanticChecker ()
increaseScope = do
  s@CheckerState{..} <- get
  put s{symbolTable = increaseScope' symbolTable}
  where
    increaseScope' (SymbolTable s [])
      = SymbolTable s [SymbolTable [] []]
    increaseScope' (SymbolTable s [c])
      = SymbolTable s [increaseScope' c]

scoped :: SemanticChecker a -> SemanticChecker a
scoped stmt
  = increaseScope *> stmt <* decreaseScope

identExists :: Identifier -> SemanticChecker ()
identExists i = void $ identLookup i

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
equalTypes _ (TString) (TPtr TChar) = valid
equalTypes _ (TPtr TChar) (TString) = valid
equalTypes errMsg (TPair t11 t12) (TPair t21 t22)
  = equalTypes errMsg t11 t21 >> equalTypes errMsg t12 t22
equalTypes errMsg (TPtr t1) (TPtr t2)
  = equalTypes errMsg t1 t2
equalTypes errMsg t1 t2
  | t1 == TArb || t2 == TArb      = valid
  | isIntType t1 && isIntType t2  = valid
  | t1 == t2                      = valid
  | otherwise                     = invalid SemanticError errMsg
