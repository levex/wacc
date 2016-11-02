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

getRegById :: Identifier -> InstructionGenerator Register
getRegById i = do
  s@InstrGenState{..} <- get
  return $ fromJust (Map.lookup i regIdsTable)

saveRegId :: Register -> Identifier -> InstructionGenerator ()
saveRegId r i = do
  s@InstrGenState{..} <- get
  put s{regIdsTable = Map.insert i r regIdsTable}

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
  saveRegId r1 id
  generateLiteral r1 lit
generateInstrForStatement (VarDef (id, t) e) = do
  r1 <- getFreeRegister
  tell [Special $ VariableDecl id t r1]
  saveRegId r1 id
  generateAssignment (Ident id) e
generateInstrForStatement (Ctrl c) = generateControl c
generateInstrForStatement (Cond e t f) = do
  elseLabel <- generateLabel
  afterCondLabel <- generateLabel
  r1 <- getFreeRegister
  generateInstrForCondExpr r1 e
  tell [Branch CNe $ Label elseLabel]
  generateInstrForStatement t
  tell [Branch CAl $ Label afterCondLabel]
  tell [Special $ LabelDecl elseLabel]
  generateInstrForStatement f
  tell [Special $ LabelDecl afterCondLabel]
generateInstrForStatement (Loop e b) = do
  beginLabel <- generateLabel
  endLabel <- generateLabel
  r1 <- getFreeRegister
  tell [Special $ LabelDecl beginLabel]
  generateInstrForCondExpr r1 e
  tell [Branch CNe $ Label endLabel]
  generateInstrForStatement b
  tell [Branch CAl $ Label beginLabel]
  tell [Special $ LabelDecl endLabel]
generateInstrForStatement (ExpStmt (BinApp Assign lhs rhs)) = do
  r1 <- getFreeRegister
  generateAssignment lhs rhs

generateControl :: Control -> InstructionGenerator ()
generateControl (Return e) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Move CAl 0 (Reg r1)]

-- #COPY#
-- data BuiltinFunc
--   = Read
--   | Free
--   | Exit
--   | Print
--   | PrintLn

-- FIXME: arg needs to be checked to be in bounds of -255 < arg < 255
generateBuiltin :: BuiltinFunc -> Expr -> InstructionGenerator ()
generateBuiltin f e = do
  generateInstrForExpr 0 e
  case f of
    Exit -> do
      tell [Move CAl 7 (Imm 1)]
      tell [Special $ SWI 0]
    _ -> skip

generateAssignment :: Expr -> Expr -> InstructionGenerator ()
generateAssignment (Ident i) e = do
  r <- getRegById i
  generateInstrForExpr r e
generateAssignment (ArrElem i idxs) r
  = skip
generateAssignment (PairElem e i) r
  = skip

generateInstrForCondExpr :: Register -> Expr -> InstructionGenerator ()
generateInstrForCondExpr r e
   = skip

generateInstrForExpr :: Register -> Expr -> InstructionGenerator ()
generateInstrForExpr r (Lit l)
  = generateLiteral r l
generateInstrForExpr r (Ident id) = do
  r1 <- getRegById id
  tell [Move CAl r (Reg r1)]
generateInstrForExpr r (ArrElem id idxs)
  = skip
generateInstrForExpr r (PairElem e id)
  = skip
generateInstrForExpr r (UnApp op e) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  case op of
    Not -> skip
    Neg -> tell [Negate CAl r (Reg r1)]
    Len -> skip
    Ord -> skip
    Chr -> skip
generateInstrForExpr r (BinApp op e1 e2) = do
  r1 <- getFreeRegister
  r2 <- getFreeRegister
  generateInstrForExpr r1 e1
  generateInstrForExpr r2 e2
  case op of
    Add -> tell [Op CAl AddOp r r1 (Reg r2)]
    Sub -> tell [Op CAl SubOp r r1 (Reg r2)]
    Mul -> tell [Op CAl MulOp r r1 (Reg r2)]
    Div -> tell [Op CAl DivOp r r1 (Reg r2)]
    Mod -> tell [Op CAl ModOp r r1 (Reg r2)]
    And -> tell [Op CAl AndOp r r1 (Reg r2)]
    Or  -> tell [Op CAl OrOp r r1 (Reg r2)]
    Gt  -> skip
    Gte -> skip
    Lt  -> skip
    Lte -> skip
    Eq  -> skip
    NEq -> skip
generateInstrForExpr r (FunCall id expr)
  = skip
generateInstrForExpr r (NewPair e1 e2)
  = skip

generateLiteral :: Register -> Literal -> InstructionGenerator ()
generateLiteral r (INT i)
  = tell [Load CAl r (Imm $ fromInteger i) True (Imm 0)]
generateLiteral r (CHAR c)
  = tell [Load CAl r (Imm $ ord c) True (Imm 0)]
generateLiteral r (BOOL b)
  = tell [Load CAl r (Imm $ bool 0 1 b) True (Imm 0)]
generateLiteral r (STR s) = do
  l <- generateLabel
  tell [Special $ StringLit l s, Load CAl r (Label $ l) True (Imm 0)]
generateLiteral r (ARRAY a)
  = skip
generateLiteral r NULL
  = skip

generateImplicitReturn :: Identifier -> InstructionGenerator ()
generateImplicitReturn "main"
  = tell [Move CAl 0 (Imm 0)]
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
  = execWriter . flip evalStateT (InstrGenState 0 0 Map.empty) . runInstrGen . mapM_ generateFunction
