module WACC.Optimizations.Expressions where

import           WACC.Parser.Types
import           WACC.Optimizations.Types

minimizeExpr :: Expr -> Optimizer Expr
minimizeExpr e
  = return e

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
  = Block <$> mapM (\(IdentifiedStatement s i)
      -> IdentifiedStatement <$> minimizeStmt s <*> return i) stmts

minimizeStmt (VarDef d e)
  = VarDef <$> return d <*> minimizeExpr e

minimizeStmt (Ctrl (Return e))
  = Ctrl <$> (Return <$> minimizeExpr e)

minimizeStmt (Cond e t f)
  = Cond <$> minimizeExpr e <*> return t <*> return f

minimizeStmt (Loop e body)
  = Loop <$> minimizeExpr e <*> return body

minimizeStmt (Builtin f e)
  = Builtin <$> return f <*> minimizeExpr e

minimizeStmt (ExpStmt e)
  = ExpStmt <$> minimizeExpr e

minimizeStmt s
  = return s

minimizeDef :: Definition -> Optimizer Definition
minimizeDef (FunDef d stmt)
  = FunDef <$> return d <*> minimizeStmt stmt

minimizeExpressions :: Program -> Optimizer Program
minimizeExpressions
  = mapM minimizeDef

