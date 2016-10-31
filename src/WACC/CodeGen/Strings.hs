module WACC.CodeGen.Strings where

import           WACC.Parser.Types
import           WACC.CodeGen.Types

skip = return ()

emitLiteral :: Literal -> CodeGenerator ()
emitLiteral (STR s)
  = skip

emitLiteral (ARRAY arr)
  = skip

emitLiteral _
  = skip

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
emitLiterals
  = mapM_ emitLiteralsFromDef
