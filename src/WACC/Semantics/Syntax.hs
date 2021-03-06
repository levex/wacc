module WACC.Semantics.Syntax where

import           Data.Int
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           WACC.Parser.Types
import           WACC.Semantics.Types
import           WACC.Semantics.Primitives

checkLit :: Literal -> SemanticChecker ()
--checkLit (INT i)
--  | inRange i = valid
--  | otherwise = invalid SyntaxError "integer out of range"
--  where
--    inRange i
--      = i >= fromIntegral (minBound :: Int32)
--        && i <= fromIntegral (maxBound :: Int32)

checkLit _
  = valid

checkExpr :: Expr -> SemanticChecker ()
checkExpr (Lit l)
  = case l of
      ARRAY _ -> invalid SyntaxError "array literals cannot occur in expression"
      _       -> checkLit l

checkExpr (Ident _)
  = valid

checkExpr (ArrElem _ exprs)
  = mapM_ checkExpr exprs

checkExpr (UnApp Neg (Lit (INT i)))
  = checkLit (INT (-i))

checkExpr (UnApp _ e)
  = checkExpr e

checkExpr (BinApp Member lhs (Ident i))
  = checkLhs lhs

checkExpr (BinApp Member _ _)
  = invalid SyntaxError "accessing invalid member of struct"

checkExpr (BinApp _ e1 e2)
  = checkExpr e1 >> checkExpr e2

checkExpr (PairElem _ _)
  = invalid SyntaxError "pair elements cannot occur in expressions"

checkExpr (FunCall _ _)
  = valid -- invalid SyntaxError "function calls cannot occur in expressions"

checkExpr (SizeOf _)
  = valid

checkExpr (OffsetOf (TStruct _) _)
  = valid

checkLhs :: Expr -> SemanticChecker ()
checkLhs (Ident _)
  = valid

checkLhs (ArrElem _ exprs)
  = mapM_ checkExpr exprs

checkLhs (PairElem _ _)
  = valid

checkLhs (UnApp Deref e)
  = valid

checkLhs e@(BinApp Member _ _)
  = checkExpr e

checkLhs _
  = invalid SyntaxError $ "lhs of an assignment must be an identifier,"
    ++ " array element or pair element"

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

checkType :: Type -> SemanticChecker ()
checkType (TPair (TPair TArb TArb) (TPair TArb TArb))
  = valid

checkType (TPair (TPair _ _) (TPair _ _))
  = invalid SyntaxError "nested pairs cannot have types"

checkType (TPair (TPair TArb TArb) t)
  = checkType t

checkType (TPair (TPair _ _) _)
  = invalid SyntaxError "nested pairs cannot have types"

checkType (TPair t (TPair TArb TArb))
  = checkType t

checkType (TPair _ (TPair _ _))
  = invalid SyntaxError "nested pairs cannot have types"

checkType (TPair TArb TArb)
  = invalid SyntaxError "outer pair must define type of elements"

checkType (TPair t1 t2)
  = checkType t1 >> checkType t2

checkType (TArray t)
  = checkType t

checkType (TPtr (TArray _))
  = invalid SyntaxError "cannot have a pointer to an array"

checkType (TPtr TArb)
  = valid

checkType (TPtr (TStruct _))
  = valid

checkType (TPtr t)
  = checkType t

checkType (TFun TArb decls)
  = mapM_ (checkType . snd) decls

checkType (TFun retT decls)
  = checkType retT >> mapM_ (checkType . snd) decls

checkType TArb
  = invalid SyntaxError "invalid void type"

checkType (TStruct _)
  = invalid SyntaxError "cannot have a struct on the stack"

checkType _
  = valid

checkStmt :: Statement -> SemanticChecker ()
checkStmt Noop
  = valid

checkStmt (Block idStmts)
  = mapM_ checkStmt idStmts

checkStmt (VarDef (_, t) e)
  = checkType t >> checkRhs e

checkStmt (Ctrl (Return (Just e)))
  = checkExpr e

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

checkStmt (ExpStmt (UnApp op e))
  | op `elem` incs = checkLhs e
  | otherwise      = checkExpr e
  where
    incs = [PreInc, PreDec, PostInc, PostDec]

checkStmt (ExpStmt (BinApp op lhs rhs))
  | op `elem` assignmentOps = checkLhs lhs >> checkRhs rhs

checkStmt (ExpStmt (BinApp op e1 e2))
  = checkExpr e1 >> checkExpr e2

checkStmt (ExpStmt (FunCall _ es))
  = mapM_ checkExpr es

checkStmt (IdentifiedStatement s i)
  = checkStmt s `catchError` rethrowWithLocation i

checkStmt (InlineAssembly _)
  = valid

checkStmt s
  = invalid SyntaxError "invalid statement"

checkDef :: Definition -> SemanticChecker ()
checkDef (FunDef (_, t) block)
  = checkType t >> checkStmt block

checkDef (TypeDef _ decls)
  = mapM_ (checkType . snd) decls

checkDef (GlobalDef (_, t) e)
  = checkType t >> checkExpr e

checkDef (ExternDef _)
  = valid

syntaxCheck :: Program -> SemanticChecker Program
syntaxCheck p
  = mapM_ checkDef p *> return p

