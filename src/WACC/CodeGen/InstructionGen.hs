{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.InstructionGen where

import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Builtin
import           WACC.CodeGen.Types

getFreeRegister :: InstructionGenerator Register
getFreeRegister = do
  s@InstrGenState{..} <- get
  put s{lastRegister = lastRegister + 1}
  return $ lastRegister + 1

generateRegisterForExpr :: Expr -> InstructionGenerator Register
generateRegisterForExpr _ = return 0 -- TOOD

generateInstrForStatement :: Statement -> InstructionGenerator ()
generateInstrForStatement Noop = return ()
generateInstrForStatement (IdentifiedStatement st _) = generateInstrForStatement st
generateInstrForStatement (Block xs) = mapM_ generateInstrForStatement xs
generateInstrForStatement (Builtin f e) = generateBuiltin f e
generateInstrForStatement (VarDef (id, t) (Lit lit)) = do
  r1 <- getFreeRegister
  tell [Special $ VariableDecl id t r1]
  case lit of
    (INT i) -> tell [Load r1 (Imm $ fromInteger i) True (Imm 0)]
    _       -> undefined

generateInstrForStatement (VarDef (id, t) expr) = do
  r1 <- getFreeRegister
  tell [Special $ VariableDecl id t r1]
  r2 <- generateRegisterForExpr expr
  tell [Move r1 (Reg r2)]

generateFunction :: Definition -> InstructionGenerator ()
generateFunction (FunDef (ident, _) stmt) = do
  tell [Special $ FunctionStart ident]
  generateInstrForStatement stmt
  tell [Special $ FunctionEnd ident]

generateInstructions :: Program -> [Instruction]
generateInstructions
  = execWriter . flip evalStateT (InstrGenState 0) . runInstrGen . mapM_ generateFunction
