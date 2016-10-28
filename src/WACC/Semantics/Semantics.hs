module WACC.Semantics.Semantics where

import           Data.Foldable
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
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
      = mapM (return . ((:) stmt)) cps

checkCodePathsReturn :: Definition -> SemanticChecker ()
checkCodePathsReturn ((ident, _), stmts) = do
  codePaths <- genCodePaths stmts
  unless (all (any isReturnOrExit) codePaths)
    $ invalid SemanticError "not all code paths return a value"

checkUnreachableCode :: Definition -> SemanticChecker ()
checkUnreachableCode ((ident, _), stmts) = do
  codePaths <- genCodePaths stmts
  when ((all (not . isReturnOrExit . last) codePaths)
        || (all ((> 1) . length . filter isReturnOrExit) codePaths))
    $ invalid SemanticError "unreachable code after return statement"

checkMainDoesNotReturn :: Definition -> SemanticChecker ()
checkMainDoesNotReturn (_, stmts) = do
  codePaths <- genCodePaths stmts
  unless (not (any (any isReturn) codePaths))
    $ invalid SemanticError "cannot return a value from the global scope"

equalTypes :: String -> Type -> Type -> SemanticChecker ()
equalTypes errMsg t1 t2
  | t1 == t2  = return ()
  | otherwise = invalid SemanticError errMsg

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
  unless (isPair tp) $ invalid SemanticError "pairElem requires a Pair"
  where
    isPair (TPair _ _) = True
    isPair p           = False
checkExpr (UnApp unop expr) = do
  checkExpr expr
  t1 <- unopType
  t2 <- getType expr
  equalTypes "undefined operation" t1 t2
  where
    unopType = (maybe undefOp $ return.snd)
             . lookup unop $ unOpTypes
checkExpr (BinApp binop e1 e2) = do
  checkExpr e1
  checkExpr e2
  te1 <- getType e1
  te2 <- getType e2
  (t1, t2) <- opTypes
  equalTypes "undefined operation" t1 te1
  equalTypes "undefined operation" t2 te2
  where
    opTypes = (maybe undefOp tp) . lookup binop $ binAppTypes
    tp = (\(_,t1,t2) -> return (t1,t2))
checkExpr (FunCall ident args) = do
  mapM_ checkExpr args
  t <- identLookup ident
  case t of
    (TFun _ params) -> checkArgs params args
    _               -> invalid SemanticError "invalid arguments"
  where
    checkArgs :: [Declaration] -> [Expr] -> SemanticChecker ()
    checkArgs params args = do
      types <- mapM getType args
      case (map snd params) == types of
        True  -> valid
        False -> invalid SemanticError "invalid arguments"
checkExpr (NewPair e1 e2)
  = checkExpr e1 >> checkExpr e2

storeDecl :: Declaration -> SemanticChecker ()
storeDecl (ident, t)
  = addSymbol (Symbol ident t)

storeFuncs :: [Definition] -> SemanticChecker ()
storeFuncs defs
  = mapM_ storeDecl $ map fst defs

semanticCheck :: Program -> SemanticChecker ()
semanticCheck (mainF:funcs) = do
  mapM_ checkCodePathsReturn funcs
  mapM_ checkUnreachableCode funcs
  checkMainDoesNotReturn mainF
  storeFuncs funcs
