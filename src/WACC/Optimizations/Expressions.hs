{-# LANGUAGE PatternSynonyms #-}
module WACC.Optimizations.Expressions where

import           Data.Char
import           WACC.Parser.Types
import           WACC.Optimizations.Types

pattern Char v = Lit (CHAR v)
pattern Int  v = Lit (INT v)
pattern Bool v = Lit (BOOL v)
pattern Str  v = Lit (STR v)
pattern Arr  v = Lit (ARRAY v)
pattern Null   = Lit NULL

pattern (:+:)   e1 e2 = BinApp Add e1 e2
pattern (:-:)   e1 e2 = BinApp Sub e1 e2
pattern (:*:)   e1 e2 = BinApp Mul e1 e2
pattern (:/:)   e1 e2 = BinApp Div e1 e2
pattern (:%:)   e1 e2 = BinApp Mod e1 e2
pattern (:&&:)  e1 e2 = BinApp And e1 e2
pattern (:||:)  e1 e2 = BinApp And e1 e2
pattern (:>:)   e1 e2 = BinApp Gt  e1 e2
pattern (:>=:)  e1 e2 = BinApp Gte e1 e2
pattern (:<:)   e1 e2 = BinApp Lt  e1 e2
pattern (:<=:)  e1 e2 = BinApp Lte e1 e2
pattern (:==:)  e1 e2 = BinApp Eq  e1 e2
pattern (:!=:)  e1 e2 = BinApp NEq e1 e2

simplify :: Expr -> Expr
simplify (UnApp Not (Bool b)) = Bool (not b)
simplify (UnApp Neg (Int n))  = Int (-n)
simplify (UnApp Len (Arr xs)) = Int (fromIntegral $ length xs)
simplify (UnApp Len (Str s))  = Int (fromIntegral $ length s)
simplify (UnApp Ord (Char c)) = Int (fromIntegral $ ord c)
simplify (UnApp Chr (Int c))  = Char (chr $ fromInteger c)

simplify (Int n1 :+: Int n2) = Int (n1 + n2)
simplify (Int 0 :+: e)       = simplify e
simplify (e :+: Int 0)       = simplify e

simplify (Int n1 :-: Int n2) = Int (n1 - n2)
simplify (Int 0 :-: e)       = UnApp Neg (simplify e)
simplify (e :-: Int 0)       = simplify e

simplify (Int n1 :*: Int n2) = Int (n1 * n2)
simplify (Int 1 :*: e)       = simplify e
simplify (e :*: Int 1)       = simplify e
simplify (Int 0 :*: e)       = Int 0
simplify (e :*: Int 0)       = Int 0

simplify (Int n1 :/: Int n2) = Int (n1 `div` n2)
simplify (Int 0 :/: e)       = Int 0
simplify (e :/: Int 1)       = simplify e

simplify (Int n1 :%: Int n2) = Int (n1 `mod` n2)
simplify (e :%: Int 1)       = Int 1
simplify (Int 0 :%: e)       = Int 0

simplify (Int n1 :*: (Int n2 :*: e)) = Int (n1 * n2) :*: simplify e
simplify (Int n1 :*: (e :*: Int n2)) = Int (n1 * n2) :*: simplify e
simplify ((Int n1 :*: e) :*: Int n2) = Int (n1 * n2) :*: simplify e
simplify ((e :*: Int n1) :*: Int n2) = Int (n1 * n2) :*: simplify e

simplify (Int n1 :+: (Int n2 :+: e)) = Int (n1 + n2) :+: simplify e
simplify (Int n1 :+: (e :+: Int n2)) = Int (n1 + n2) :+: simplify e
simplify ((Int n1 :+: e) :+: Int n2) = Int (n1 + n2) :+: simplify e
simplify ((e :+: Int n1) :+: Int n2) = Int (n1 + n2) :+: simplify e

simplify (Bool True :&&: b)  = simplify b
simplify (Bool False :&&: _) = Bool False
simplify (b :&&: Bool True)  = simplify b
simplify (_ :&&: Bool False) = Bool False

simplify (Bool True :||: _)  = Bool True
simplify (Bool False :&&: b) = simplify b
simplify (_ :&&: Bool True)  = Bool True
simplify (b :&&: Bool False) = simplify b

simplify (Int n1 :>: Int n2)    = Bool (n1 > n2)
simplify (Char c1 :>: Char c2)  = Bool (c1 > c2)

simplify (Int n1 :>=: Int n2)   = Bool (n1 >= n2)
simplify (Char c1 :>=: Char c2) = Bool (c1 >= c2)

simplify (Int n1 :<: Int n2)    = Bool (n1 < n2)
simplify (Char c1 :<: Char c2)  = Bool (c1 < c2)

simplify (Int n1 :<=: Int n2)   = Bool (n1 <= n2)
simplify (Char c1 :<=: Char c2) = Bool (c1 <= c2)

simplify (Lit l1 :==: Lit l2)   = Bool (l1 == l2)
simplify (Lit l1 :!=: Lit l2)   = Bool (l1 /= l2)

simplify (ArrElem i indices)  = ArrElem i (map simplify indices)
simplify (UnApp op e)         = UnApp op (simplify e)
simplify (BinApp op e1 e2)    = BinApp op (simplify e1) (simplify e2)
simplify (FunCall i args)     = FunCall i (map simplify args)
simplify (NewPair e1 e2)      = NewPair (simplify e1) (simplify e2)

simplify e  = e

minimize curr last
  | curr == last  = curr
  | otherwise     = minimize (simplify curr) curr

minimizeExpr :: Expr -> Optimizer Expr
minimizeExpr e
  = return $ minimize e Null

--  = Lit Literal
--  | Ident Identifier
--  | ArrElem Identifier [Expr]
--  | PairElem PairElement Identifier
--  | UnApp UnOp Expr
--  | BinApp BinOp Expr Expr
--  | FunCall Identifier [Expr]
--  | NewPair Expr Expr

minimizeStmt :: Statement -> Optimizer Statement
minimizeStmt (Block stmts)
  = Block <$> mapM minimizeStmt stmts

minimizeStmt (VarDef d e)
  = VarDef <$> return d <*> minimizeExpr e

minimizeStmt (Ctrl (Return e))
  = Ctrl <$> (Return <$> minimizeExpr e)

minimizeStmt (Cond e t f)
  = Cond <$> minimizeExpr e <*> minimizeStmt t <*> minimizeStmt f

minimizeStmt (Loop e body)
  = Loop <$> minimizeExpr e <*> minimizeStmt body

minimizeStmt (Builtin f e)
  = Builtin <$> return f <*> minimizeExpr e

minimizeStmt (ExpStmt e)
  = ExpStmt <$> minimizeExpr e

minimizeStmt (IdentifiedStatement s i)
  = IdentifiedStatement <$> minimizeStmt s <*> return i

minimizeStmt s
  = return s

minimizeDef :: Definition -> Optimizer Definition
minimizeDef (FunDef d stmt)
  = FunDef <$> return d <*> minimizeStmt stmt

minimizeExpressions :: Program -> Optimizer Program
minimizeExpressions
  = mapM minimizeDef

