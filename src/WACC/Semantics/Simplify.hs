module WACC.Semantics.Simplify where

import           WACC.Parser.Types
import           WACC.Semantics.Types
import           WACC.Semantics.Typing

getBuiltinName :: BuiltinFunc -> Type -> Identifier
getBuiltinName f t
  = concat ["__builtin_", show f, "_", builtinForType t]
  where
    builtinForType (TArray _)
      = "TRef"

    builtinForType (TPair _ _)
      = "TRef"

    builtinForType t
      = show t

simplifyStmt :: Statement -> SemanticChecker Statement
simplifyStmt b@(Builtin Exit _)
  = return b

simplifyStmt b@(Builtin Free _)
  = return b

simplifyStmt (Builtin PrintLn e) = do
  s <- simplifyStmt (Builtin Print e)
  return $ Block [s, ExpStmt (FunCall "__builtin_println" [])]

simplifyStmt (Builtin f e) = do
  t <- getType e
  return $ ExpStmt (FunCall (getBuiltinName f t) [e])

simplifyStmt (IdentifiedStatement s i)
  = simplifyStmt s

simplifyStmt (Block stmts)
  = scoped $ Block <$> mapM simplifyStmt stmts

simplifyStmt (Cond e trueBranch falseBranch)
  = Cond e <$> simplifyStmt trueBranch <*> simplifyStmt falseBranch

simplifyStmt (Loop e body)
  = Loop e <$> simplifyStmt body

simplifyStmt s@(VarDef (id, t) _)
  = addSymbol (Symbol id t) >> return s

simplifyStmt s
  = return s

simplifyDef :: Definition -> SemanticChecker Definition
simplifyDef (FunDef decl block)
  = FunDef decl <$> simplifyStmt block

simplify :: Program -> SemanticChecker Program
simplify
  = mapM simplifyDef
