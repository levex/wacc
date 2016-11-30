{-# LANGUAGE RecordWildCards #-}
module WACC.Semantics.Semantics where

import           Data.Foldable
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map as Map
import           Data.Map (Map)
import           WACC.Semantics.Types
import           WACC.Semantics.Primitives
import           WACC.Semantics.Typing
import           WACC.Parser.Types

genCodePaths :: Statement -> SemanticChecker [[Statement]]
genCodePaths stmts
  = genCodePath stmts [[]]
  where
    genCodePath :: Statement -> [[Statement]] -> SemanticChecker [[Statement]]
    genCodePath (Block stmts) cps
      = foldrM (\(IdentifiedStatement s _) c -> genCodePath s c) cps stmts
    -- TODO: further optimizations and checks can be made for Conds and Loops
    -- by minimizing constant expressions (e.g. we can check if an `if` branch
    -- will ever be entered)
    genCodePath (Cond _ trueBranch falseBranch) cps
      = liftM2 (++) (genCodePath trueBranch cps) (genCodePath falseBranch cps)
    genCodePath (Loop _ body) cps
      = liftM2 (++) (genCodePath body cps) (return cps)
    genCodePath stmt cps
      = mapM (return . (:) stmt) cps


checkCodePathsReturn :: Definition -> SemanticChecker ()
checkCodePathsReturn (FunDef (_, TFun TArb _) _)
  = valid
checkCodePathsReturn (FunDef _ stmts) = do
  codePaths <- genCodePaths stmts
  unless (all (any isReturnOrExit) codePaths)
    $ invalid SemanticError "not all code paths return a value"
checkCodePathsReturn _
  = valid


checkUnreachableCode :: Definition -> SemanticChecker ()
checkUnreachableCode (FunDef (_, TFun TArb _) stmts)
  = valid
checkUnreachableCode (FunDef _ stmts) = do
  codePaths <- genCodePaths stmts
  when ((all (not . isReturnOrExit . last) codePaths)
        || (all ((> 1) . length . filter isReturnOrExit) codePaths))
    $ invalid SemanticError "unreachable code after return statement"
checkUnreachableCode _
  = valid


checkDef :: Definition -> SemanticChecker ()
checkDef (FunDef (ident, TFun rT paramT) stmt) = scoped $ do
  mapM_ storeDecl paramT
  storeDecl ("%RETURN%", rT)
  checkStmt stmt
checkDef _
  = valid


checkExpr :: Expr -> SemanticChecker ()
checkExpr (Lit lit)
  = valid
checkExpr (Ident ident)
  = identExists ident
checkExpr (ArrElem ident [])
  = invalid SemanticError "invalid array index"
checkExpr (ArrElem ident idx) = do
  mapM_ checkExpr idx
  types <- mapM getType idx
  mapM_ (equalTypes "array index must be an integer" TInt) types
checkExpr (PairElem pairElem ident) = do
  tp <- identLookup ident
  equalTypes "type error: pairElem requires a Pair" tp (TPair TArb TArb)
checkExpr (UnApp unop expr) = do
  checkExpr expr
  a <- getType expr
  checkUnopArgs unop a
checkExpr e@(BinApp Member _ _)
  = getType e >> valid
checkExpr (BinApp binop e1 e2) = do
  checkExpr e1
  checkExpr e2
  a1 <- getType e1
  a2 <- getType e2
  checkBinopArgs binop a1 a2
checkExpr (FunCall ident args) = do
  mapM_ checkExpr args
  t <- identLookup ident
  case t of
    (TFun _ params) -> checkArgs params args
    _               -> invalid SemanticError $ "function " ++ show ident ++ " doesn't exist"
  where
    checkArgs :: [Declaration] -> [Expr] -> SemanticChecker ()
    checkArgs params args = do
      types <- mapM getType args
      if sameLength (map snd params) types
        then zipWithM_ (equalTypes "invalid argument") (map snd params) types
        else invalid SemanticError "invalid number of arguments"
checkExpr (NewPair e1 e2)
  = checkExpr e1 >> checkExpr e2
checkExpr (SizeOf _)
  = valid

checkStmt :: Statement -> SemanticChecker ()
checkStmt Noop
  = valid
checkStmt (Block idStmts)
  = scoped $ mapM_ checkStmt idStmts
checkStmt (VarDef decl@(_, declT) expr) = do
  checkExpr expr
  t <- getType expr
  equalTypes ("type Mismatch " ++ show (snd decl) ++ " vs " ++ show t) declT t
  storeDecl decl
checkStmt (Ctrl Break)
  = valid
checkStmt (Ctrl Continue)
  = valid
checkStmt (InlineAssembly _)
  = valid
checkStmt (Ctrl (Return (Just e))) = do
  checkExpr e
  rT <- identLookup "%RETURN%"
  t <- getType e
  equalTypes "return type must be the same as the function type" rT t
checkStmt (Ctrl (Return Nothing)) = do
  rT <- identLookup "%RETURN%"
  equalTypes "no return from a non-void function" rT TArb
checkStmt (Cond e s1 s2) = do
  checkExpr e
  t <- getType e
  equalTypes "type error: condition must be of type bool" TBool t
  scoped (checkStmt s1) >> scoped (checkStmt s2)
checkStmt (Loop cond stmt) = do
  checkExpr cond
  t <- getType cond
  equalTypes "type mismatch" TBool t
  scoped $ checkStmt stmt
checkStmt (Builtin func ex) = do
  checkExpr ex
  checkBuiltinArg func ex
  where
    checkBuiltinArg Read e = do
      t <- getType e
      isIntChar t
      where
        isIntChar TInt  = valid
        isIntChar TChar = valid
        isIntChar _       = invalid SemanticError "type error"
    checkBuiltinArg Free e = do
      t <- getType e
      case t of
        (TPair _ _) -> valid
        (TArray _)  -> valid
        _           -> invalid SemanticError "invalid argument"
    checkBuiltinArg Exit e = do
      t <- getType e
      equalTypes "invalid argument" TInt t
    checkBuiltinArg Print e = valid
    checkBuiltinArg PrintLn e = valid
checkStmt (ExpStmt e)
  = checkExpr e
checkStmt (IdentifiedStatement stmt i)
  = checkStmt stmt `catchError` rethrowWithLocation i

storeGlobals :: [Definition] -> SemanticChecker ()
storeGlobals
  = mapM_ (storeDecl . getDecl)

storeStruct :: Definition -> SemanticChecker ()
storeStruct (TypeDef ident members) = do
  s@CheckerState{..} <- get
  put s{structDefs = Map.insert ident members structDefs}
storeStruct _
  = valid

storeStructs :: [Definition] -> SemanticChecker ()
storeStructs
  = mapM_ storeStruct

semanticCheck :: Program -> SemanticChecker Program
semanticCheck p = do
  mapM_ checkCodePathsReturn p
  mapM_ checkUnreachableCode p
  storeStructs p
  storeGlobals p
  mapM_ checkDef p
  return p
