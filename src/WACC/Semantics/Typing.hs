module WACC.Semantics.Typing where

import Data.Maybe

import WACC.Parser.Types
import WACC.Parser.Primitives

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


getLiteralType :: Literal -> Type
getLiteralType (CHAR _) = TChar
getLiteralType (INT _) = TInt
getLiteralType (BOOL _) = TBool
getLiteralType (STR _) = TString
getLiteralType (ARRAY (e : _)) = TArray (getType e)

deconstructArrayType :: Type -> Type
deconstructArrayType (TArray t) = t

-- FIXME: instead of maybe throwing undefined, return an error
getType :: Expr -> Type
getType (Lit lit)
  = getLiteralType lit
getType (Ident id)
  = undefined -- FIXME: need symbol table
getType (ArrElem id _)
  = deconstructArrayType $ getType (Ident id)
getType (PairElem e id)
  = case getType (Ident id) of
      (TPair f s) -> pairElem e f s
      _ -> undefined -- FIXME: type error
  where
    pairElem Fst f _ = f
    pairElem Snd _ s = s
getType (UnApp op _)
  = snd . fromMaybe undefined . lookup op $ unOpTypes
getType (BinApp op _ _)
  = (\(_,_,x) -> x) . fromMaybe undefined . lookup op $ binAppTypes
getType (FunCall id _)
  = undefined -- FIXME: need symbol table
getType (NewPair e1 e2)
  = TPair (getType e1) (getType e2) -- FIXME: pair<T1,T2>
