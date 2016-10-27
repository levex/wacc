module WACC.Semantics.Typing where

import Data.Maybe

import WACC.Parser.Types
import WACC.Parser.Primitives
import WACC.Semantics.Semantics
import WACC.Semantics.Types

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

getLiteralType :: Literal -> SymbolTable -> Maybe Type
getLiteralType (CHAR _) _ = Just TChar
getLiteralType (INT _) _  = Just TInt
getLiteralType (BOOL _) _ = Just TBool
getLiteralType (STR _) _  = Just TString
getLiteralType (ARRAY (e : _)) st
  = case getType e st of
      (Just t) -> Just (TArray t)
      _        -> Nothing

deconstructArrayType :: Maybe Type -> Maybe Type
deconstructArrayType (Just (TArray t)) = Just t
deconstructArrayType t                 = Nothing

getType :: Expr -> SymbolTable -> Maybe Type
getType (Lit lit) st
  = getLiteralType lit st
getType (Ident id) st
  = getTypeForId id st
getType (ArrElem id _) st
  = deconstructArrayType $ getType (Ident id) st
getType (PairElem e id) st
  = case getType (Ident id) st of
      (Just (TPair f s)) -> Just (pairElem e f s)
      _                  -> Nothing
  where
    pairElem Fst f _ = f
    pairElem Snd _ s = s
getType (UnApp op _) _
  = (maybe Nothing (\(_, x) -> (Just x))) . lookup op $ unOpTypes
getType (BinApp op _ _) _
  = (maybe Nothing (\(_,_,x) -> (Just x))) . lookup op $ binAppTypes
getType (FunCall id _) st
  = getTypeForId id st
getType (NewPair e1 e2) st
  = Just (TPair t1 t2) -- FIXME: pair<T1,T2>
  where
    (Just t1) = getType e1 st
    (Just t2) = getType e2 st
