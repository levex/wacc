module WACC.Semantics.Syntax where

import           Data.Int
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           WACC.Parser.Types
import           WACC.Semantics.Types

valid :: SemanticChecker ()
valid
  = return ()

invalid :: SemanticChecker ()
invalid
  = throwError $ CheckerError SyntaxError (Location 0 0)

checkLit :: Literal -> SemanticChecker ()
checkLit (INT i)
  | inRange i = valid
  | otherwise = invalid
  where
    inRange i
      = i >= fromIntegral (minBound :: Int32)
        && i <= fromIntegral (maxBound :: Int32)

checkLit _
  = valid

checkExpr :: Expr -> SemanticChecker ()
checkExpr (Lit l)
  = case l of
      ARRAY _ -> invalid
      _       -> checkLit l

checkExpr (Ident _)
  = valid

checkExpr (ArrElem _ exprs)
  = mapM_ checkExpr exprs

checkExpr (UnApp Neg (Lit (INT i)))
  = checkLit (INT (-i))

checkExpr (UnApp _ e)
  = checkExpr e

checkExpr (BinApp _ e1 e2)
  = checkExpr e1 >> checkExpr e2

checkExpr _
  = invalid

checkLhs :: Expr -> SemanticChecker ()
checkLhs (Ident _)
  = valid

checkLhs (ArrElem _ exprs)
  = mapM_ checkExpr exprs

checkLhs (PairElem _ _)
  = valid

checkLhs _
  = invalid

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
  = invalid

checkIdStmt :: IdentifiedStatement -> SemanticChecker ()
checkIdStmt (IdentifiedStatement s i)
  = checkStmt s `catchError` rethrowWithLocation
  where
    rethrowWithLocation :: CheckerError -> SemanticChecker ()
    rethrowWithLocation (CheckerError e _) = do
      ld <- gets locationData
      let loc = fromJust $ Map.lookup i (locations ld)
      throwError $ CheckerError e loc

checkDef :: Definition -> SemanticChecker ()
checkDef (_, block)
  = checkStmt block

syntaxCheck :: Program -> SemanticChecker ()
syntaxCheck
  = mapM_ checkDef
