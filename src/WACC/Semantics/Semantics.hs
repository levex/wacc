module WACC.Semantics.Semantics where

import           Data.Foldable
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except
import           WACC.Semantics.Types
import           WACC.Semantics.Primitives
import           WACC.Parser.Types

addSymbol :: Symbol -> SymbolTable -> SymbolTable
addSymbol s (SymbolTable ss [])
  = (SymbolTable (s:ss) [])
addSymbol s (SymbolTable ss [c])
  = (SymbolTable ss [addSymbol s c])

decreaseScope :: SymbolTable -> SymbolTable
decreaseScope (SymbolTable s [(SymbolTable ss [])])
  = SymbolTable s []
decreaseScope (SymbolTable s [c])
  = (SymbolTable s [decreaseScope c])
decreaseScope st
  = SymbolTable [] []

increaseScope :: SymbolTable -> SymbolTable
increaseScope (SymbolTable s [])
  = (SymbolTable s [(SymbolTable [] [])])
increaseScope (SymbolTable s [c])
  = (SymbolTable s [increaseScope c])

identLookup :: Identifier -> SemanticChecker Type
identLookup i = do
  st <- gets symbolTable
  case getTypeForId i st of
    (Just t) -> return t
    Nothing  -> invalid SemanticError "undefined identifier"

getTypeForId :: Identifier -> SymbolTable -> Maybe Type
getTypeForId i (SymbolTable s [])
  = lookup i (map (\(Symbol i t) -> (i, t)) s)
getTypeForId i (SymbolTable s [ch])
  | res /= Nothing = res
  | otherwise      = getTypeForId i (SymbolTable s [])
  where
    res = getTypeForId i ch

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

codePathReturns :: [Statement] -> Bool
codePathReturns
  = any isReturn
  where
    isReturn (Ctrl (Return _))
      = True

    isReturn (Builtin Exit _)
      = True

    isReturn _
      = False

checkCodePathsReturn :: Definition -> SemanticChecker ()
checkCodePathsReturn ((ident, _), stmts) = do
  codePaths <- genCodePaths stmts
  unless (all codePathReturns codePaths)
    $ invalid SemanticError "not all code paths return a value"

semanticCheck :: Program -> SemanticChecker ()
semanticCheck (mainF:funcs)
  = mapM_ checkCodePathsReturn funcs
