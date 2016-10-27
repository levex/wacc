module WACC.Semantics.Syntax where

import           Control.Monad
import           Control.Monad.Except
import           WACC.Parser.Types
import           WACC.Semantics.Types

valid :: SemanticChecker ()
valid
  = return ()

checkExpr :: Expr -> SemanticChecker ()
checkExpr (Lit l)
  = case l of
      ARRAY _ -> throwError SyntaxError
      _       -> valid

checkExpr (Ident _)
  = valid

checkExpr (ArrElem _ exprs)
  = mapM_ checkExpr exprs

checkExpr (UnApp _ e)
  = checkExpr e

checkExpr (BinApp _ e1 e2)
  = checkExpr e1 >> checkExpr e2

checkExpr _
  = throwError SyntaxError

checkLhs :: Expr -> SemanticChecker ()
checkLhs (Ident _)
  = valid

checkLhs (ArrElem _ exprs)
  = mapM_ checkExpr exprs

checkLhs (PairElem _ _)
  = valid

checkLhs _
  = throwError SyntaxError

checkRhs :: Expr -> SemanticChecker ()
checkRhs (Lit (ARRAY _))
  = valid

checkRhs (PairElem _ _)
  = valid

checkRhs (FunCall ident args)
  = valid

checkRhs (NewPair e1 e2)
  = checkExpr e1 >> checkExpr e2

checkRhs e
  = checkExpr e

checkStmt :: Statement -> SemanticChecker ()
checkStmt Noop
  = valid

checkStmt (Block idStmts)
  = mapM_ checkIdStmt idStmts

checkStmt (VarDef _ e)
  = checkRhs e

checkStmt (Ctrl _)
  = valid

checkStmt (Cond e trueBranch falseBranch)
  = checkExpr e >> checkStmt trueBranch >> checkStmt falseBranch

checkStmt (Loop e body)
  = checkExpr e >> checkStmt body

checkStmt (Builtin Read e)
  = checkLhs e

checkStmt (Builtin _ e)
  = checkExpr e

checkStmt (ExpStmt (BinApp Assign lhs rhs))
  = checkLhs lhs >> checkRhs rhs

checkStmt s
  = throwError SyntaxError

checkIdStmt :: IdentifiedStatement -> SemanticChecker ()
checkIdStmt (IdentifiedStatement s _)
  = checkStmt s

checkDef :: Definition -> SemanticChecker ()
checkDef (_, block)
  = checkStmt block

syntaxCheck :: Program -> SemanticChecker ()
syntaxCheck program
  = mapM_ checkDef program
