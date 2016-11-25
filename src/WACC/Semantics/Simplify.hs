module WACC.Semantics.Simplify where

import           WACC.Parser.Types
import           WACC.Semantics.Types
import           WACC.Semantics.Typing

getBuiltinName :: BuiltinFunc -> Maybe Type -> Identifier
getBuiltinName f (Just t)
  = concat ["__builtin_", show f, "_", builtinForType t]
  where
    builtinForType (TArray TChar)
      = "TCharArray"

    builtinForType (TArray _)
      = "TRef"

    builtinForType (TPair _ _)
      = "TRef"

    builtinForType t
      = show t
getBuiltinName f Nothing
  = "__builtin_" ++ show f

simplifyStmt :: Statement -> SemanticChecker Statement
simplifyStmt (Builtin Exit e)
  = return $ ExpStmt (FunCall (getBuiltinName Exit Nothing) [e])

simplifyStmt (Builtin Free e)
  = return $ ExpStmt (FunCall (getBuiltinName Free Nothing) [e])

simplifyStmt (Builtin PrintLn e) = do
  s <- simplifyStmt (Builtin Print e)
  return $ Block [s, ExpStmt (FunCall "__builtin_PrintLn" [])]

simplifyStmt (Builtin f e) = do
  t <- getType e
  return $ ExpStmt (FunCall (getBuiltinName f (Just t)) [e])

simplifyStmt (IdentifiedStatement s i)
  = simplifyStmt s

simplifyStmt (Block stmts)
  = scoped $ Block <$> mapM simplifyStmt stmts

simplifyStmt (Cond e trueBranch falseBranch)
  = Cond e <$> simplifyStmt trueBranch <*> simplifyStmt falseBranch

simplifyStmt (Loop e body)
  = Loop e <$> simplifyStmt body

simplifyStmt s@(VarDef d _)
  = storeDecl d >> return s

simplifyStmt s
  = return s

simplifyDef :: Definition -> SemanticChecker Definition
simplifyDef (FunDef d@(_, TFun rT paramTs) block)
  = scoped $ mapM_ storeDecl paramTs >> FunDef d <$> simplifyStmt block

simplify :: Program -> SemanticChecker Program
simplify
  = mapM simplifyDef
