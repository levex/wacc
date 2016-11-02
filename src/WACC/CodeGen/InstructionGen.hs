{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.InstructionGen where

import           Data.Bool
import           Data.Char
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

getFreeRegister :: InstructionGenerator Register
getFreeRegister = do
  s@InstrGenState{..} <- get
  put s{lastRegister = lastRegister + 1}
  return $ lastRegister + 1

generateLabel :: InstructionGenerator String
generateLabel = do
  s@InstrGenState{..} <- get
  put s{lastLabelId = lastLabelId + 1}
  return $ "__label_" ++ show lastLabelId

generateInstrForStatement :: Statement -> InstructionGenerator ()
generateInstrForStatement Noop = return ()
generateInstrForStatement (IdentifiedStatement st _) = generateInstrForStatement st
generateInstrForStatement (Block xs) = mapM_ generateInstrForStatement xs
generateInstrForStatement (Builtin f e) = generateBuiltin f e
generateInstrForStatement (VarDef (id, t) (Lit lit)) = do
  r1 <- getFreeRegister
  tell [Special $ VariableDecl id t r1]
  case lit of
    INT i   -> tell [Load r1 (Imm $ fromInteger i) True (Imm 0)]
    CHAR c  -> tell [Load r1 (Imm $ ord c) True (Imm 0)]
    BOOL b  -> tell [Load r1 (Imm $ bool 0 1 b) True (Imm 0)]
    STR s   -> tell [Special $ StringLit id s,
                     Load r1 (Label $ id) True (Imm 0)]
    ARRAY a -> skip
    NULL    -> skip
generateInstrForStatement (VarDef (id, t) expr) = do
  r1 <- getFreeRegister
  tell [Special $ VariableDecl id t r1]
  generateInstrForRhs r1 e
generateInstrForStatement (Ctrl c) = generateControl c
generateInstrForStatement (Cond e t f) = do
  elseLabel <- generateLabel
  afterCondLabel <- generateLabel
  r1 <- getFreeRegister
  generateInstrForCondExpr r1 e
  tell [Branch $ Label elseLabel]
  generateInstrForStatement t
  tell [Branch $ Label afterCondLabel]
  tell [Special $ LabelDecl elseLabel]
  generateInstrForStatement f
  tell [Special $ LabelDecl afterCondLabel]
generateInstrForStatement (Loop e b) = do
  beginLabel <- generateLabel
  endLabel <- generateLabel
  r1 <- getFreeRegister
  tell [Special $ LabelDecl beginLabel]
  generateInstrForCondExpr r1 e
  tell [Branch $ Label endLabel]
  generateInstrForStatement b
  tell [Branch $ Label beginLabel]
  tell [Special $ LabelDecl endLabel]
generateInstrForStatement (ExpStmt (BinApp Assign lhs rhs)) = do
  r1 <- getFreeRegister
  generateInstrForRhs r1 rhs
  generateAssignment lhs r1

generateControl :: Control -> InstructionGenerator ()
generateControl (Return e) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Move 0 (Reg r1)]

-- #COPY#
-- data BuiltinFunc
--   = Read
--   | Free
--   | Exit
--   | Print
--   | PrintLn

-- FIXME: arg needs to be checked to be in bounds of -255 < arg < 255
generateBuiltin :: BuiltinFunc -> Expr -> InstructionGenerator ()
generateBuiltin Exit e = do
  generateInstrForExpr 0 e
  tell [Move 7 (Imm 1)]
  tell [Special $ SWI 0]

generateBuiltin _ _
  = skip

generateInstrForRhs :: Register -> Expr -> InstructionGenerator ()
generateInstrForRhs r e
  = skip

generateAssignment :: Expr -> Register -> InstructionGenerator ()
generateAssignment e r
  = skip

generateInstrForCondExpr :: Register -> Expr -> InstructionGenerator ()
generateInstrForCondExpr r e
   = skip

generateInstrForExpr :: Register -> Expr -> InstructionGenerator ()
generateInstrForExpr r e
  = skip

generateImplicitReturn :: Identifier -> InstructionGenerator ()
generateImplicitReturn "main"
  = tell [Move 0 (Imm 0)]
generateImplicitReturn _
  = skip

generateFunction :: Definition -> InstructionGenerator ()
generateFunction (FunDef (ident, _) stmt) = do
  tell [Special $ FunctionStart ident]
  generateInstrForStatement stmt
  generateImplicitReturn ident
  tell [Special $ FunctionEnd ident]

generateInstructions :: Program -> [Instruction]
generateInstructions
  = execWriter . flip evalStateT (InstrGenState 0 0) . runInstrGen . mapM_ generateFunction
