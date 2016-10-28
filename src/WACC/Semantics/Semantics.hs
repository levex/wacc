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

semanticCheck :: Program -> SemanticChecker ()
semanticCheck (mainF:funcs) = do
  mapM_ checkCodePathsReturn funcs
  mapM_ checkUnreachableCode funcs
  checkMainDoesNotReturn mainF
