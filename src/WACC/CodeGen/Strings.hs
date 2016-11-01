{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.Strings where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

skip = return ()

createNewLiteralId :: CodeGenerator Int
createNewLiteralId = do
  s@CodeGenState{..} <- get
  put $ s { lastLiteralId = lastLiteralId + 1 }
  return $ lastLiteralId + 1

emitLiteral :: Literal -> CodeGenerator ()
emitLiteral (STR s) = do
  id <- createNewLiteralId
  emitStringLiteral ("__str_lit_" ++ show id) (Lit (STR s))

emitLiteral (ARRAY arr)
  = skip

emitLiteral _
  = skip

emitStringLiteral :: Identifier -> Expr -> CodeGenerator ()
emitStringLiteral id (Lit (STR str)) = do
  tell ["  ", id, ":\n"]
  tell ["    ", ".word ", show . length $ str, "\n"]
  tell ["    ", ".asciz \"", str, "\"\n"]

emitLiteralsFromExpr :: Expr -> CodeGenerator ()
emitLiteralsFromExpr (Lit l)            = emitLiteral l
emitLiteralsFromExpr (Ident _)          = skip
emitLiteralsFromExpr (ArrElem _ es)     = mapM_ emitLiteralsFromExpr es
emitLiteralsFromExpr (PairElem _ _)     = skip
emitLiteralsFromExpr (UnApp _ e)        = emitLiteralsFromExpr e
emitLiteralsFromExpr (BinApp _ e1 e2)   = emitLiteralsFromExpr e1 >>
                                          emitLiteralsFromExpr e2
emitLiteralsFromExpr (FunCall _ es)     = mapM_ emitLiteralsFromExpr es
emitLiteralsFromExpr (NewPair e1 e2)    = emitLiteralsFromExpr e1 >>
                                          emitLiteralsFromExpr e2

emitLiteralsFromStmt :: Statement -> CodeGenerator ()
emitLiteralsFromStmt (Noop)                     = skip
emitLiteralsFromStmt (Block ss)                 = mapM_ emitLiteralsFromStmt ss
emitLiteralsFromStmt (VarDef (id, TString) str) = emitStringLiteral id str
emitLiteralsFromStmt (VarDef _ e)               = emitLiteralsFromExpr e
emitLiteralsFromStmt (Ctrl _)                   = skip
emitLiteralsFromStmt (Cond e ts fs)             = emitLiteralsFromExpr e >>
                                                  emitLiteralsFromStmt ts >>
                                                  emitLiteralsFromStmt fs
emitLiteralsFromStmt (Loop e body)              = emitLiteralsFromExpr e >>
                                                  emitLiteralsFromStmt body
emitLiteralsFromStmt (Builtin _ e)              = emitLiteralsFromExpr e
emitLiteralsFromStmt (ExpStmt e)                = emitLiteralsFromExpr e
emitLiteralsFromStmt (IdentifiedStatement s _)  = emitLiteralsFromStmt s

emitLiteralsFromDef :: Definition -> CodeGenerator ()
emitLiteralsFromDef (FunDef _ s) = emitLiteralsFromStmt s
--emitLiteralsFromDef _ = skip

emitLiterals :: Program -> CodeGenerator ()
emitLiterals p = do
  tell [".section \".data\"\n"]
  mapM_ emitLiteralsFromDef p
