module WACC.Parser.Semantic where

import Data.Maybe

import WACC.Parser.Types
import WACC.Parser.Primitives

-- Type stuff
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
getType (PairElem Fst (NewPair e _))
  = getType e
getType (PairElem Snd (NewPair _ e))
  = getType e
getType (PairElem _ _)
  = undefined -- FIXME: Type Error
getType (UnApp op _)
  = snd . (maybe undefined id) . lookup op $ unOpTypes 
getType (BinApp op _ _)
  = (\(_,_,x) -> x) . (maybe undefined id) . lookup op $ binAppTypes
getType (FunCall id _)
  = undefined -- FIXME: need symbol table
getType (NewPair e1 e2)
  = TPair (getType e1) (getType e2) -- FIXME: pair<T1,T2>
