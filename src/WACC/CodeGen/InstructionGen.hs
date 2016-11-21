{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.InstructionGen where

import Debug.Trace
import           Data.Bool
import           Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types
import           WACC.CodeGen.LinearScanRegisterAlloc

getFreeRegister :: InstructionGenerator Register
getFreeRegister = do
  s@CodeGenState{..} <- get
  put s{lastRegister = lastRegister + 1}
  return $ R lastRegister

resetFreeRegisters :: InstructionGenerator ()
resetFreeRegisters
  = modify (\s@CodeGenState{..} -> s{lastRegister = 0})

getRegById :: Identifier -> InstructionGenerator Register
getRegById i
  = gets scopeId >>= getRegById' i
  where
    getRegById' :: Identifier -> Integer -> InstructionGenerator Register
    getRegById' i sId = do
      table <- gets regIdsTable
      case Map.lookup (i, sId) table of
        Just r  -> return r
        Nothing -> getRegById' i (sId - 1)

saveRegId :: Register -> Identifier -> InstructionGenerator ()
saveRegId r i = do
  s@CodeGenState{..} <- get
  put s{regIdsTable = Map.insert (i, scopeId) r regIdsTable}

increaseScope :: InstructionGenerator ()
increaseScope
  = modify (\s@CodeGenState{..} -> s{scopeId = scopeId + 1})

decreaseScope :: InstructionGenerator ()
decreaseScope
  = modify (\s@CodeGenState{..} -> s{scopeId = scopeId - 1})

scoped :: InstructionGenerator a -> InstructionGenerator a
scoped p
  = increaseScope *> p <* decreaseScope

generateLabel :: InstructionGenerator String
generateLabel = do
  s@CodeGenState{..} <- get
  put s{lastLabelId = lastLabelId + 1}
  return $ "__label_" ++ show lastLabelId

saveBuiltinId :: Identifier -> InstructionGenerator ()
saveBuiltinId id
  = modify (\s@CodeGenState{..} ->
      s{usedBuiltins = Set.insert id usedBuiltins})

generateInstrForStatement :: Statement -> InstructionGenerator ()
generateInstrForStatement Noop = return ()
generateInstrForStatement (Block xs) = scoped $ mapM_ generateInstrForStatement xs
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
  generateInstrForExpr r1 e
  tell [Compare CAl r1 (Imm 0)]
  tell [Branch CEq $ Label elseLabel]
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
  generateInstrForExpr r1 e
  tell [Compare CAl r1 (Imm 0)]
  tell [Branch CEq $ Label endLabel]
  generateInstrForStatement b
  tell [Branch CAl $ Label beginLabel]
  tell [Special $ LabelDecl endLabel]
generateInstrForStatement (ExpStmt (BinApp Assign lhs rhs))
  = generateAssignment lhs rhs
generateInstrForStatement (ExpStmt builtinCall@(FunCall id _))
  = generateInstrForFunCall builtinCall >> saveBuiltinId id

generateControl :: Control -> InstructionGenerator ()
generateControl (Return e) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Ret (Reg r1)]

generateAddressDerefImm :: Register -> Int -> InstructionGenerator ()
generateAddressDerefImm r offset
  = tell [Load CAl r (Reg r) True (Imm offset)]

generateAddressDeref :: Register -> Register -> InstructionGenerator ()
generateAddressDeref r offsetR
  = tell [Load CAl r (Reg r) True (Reg offsetR)]

generateArrayIndex :: Register -> Expr -> InstructionGenerator ()
generateArrayIndex r e = do
  generateInstrForExpr r e
  tell [Op CAl AddOp r r (Imm 1), Shift CAl r r LSL 2]

generateArrayDeref :: Register -> Expr -> InstructionGenerator ()
generateArrayDeref r offset = do
  offsetR <- getFreeRegister
  generateArrayIndex offsetR offset
  generateAddressDeref r offsetR

generateAssignment :: Expr -> Expr -> InstructionGenerator ()
generateAssignment (Ident i) e = do
  r <- getRegById i
  generateInstrForExpr r e
generateAssignment (ArrElem i idxs) e = do
  r <- getFreeRegister
  r1 <- getRegById i
  tell [Move CAl r (Reg r1)]
  let idxCount = length idxs
  let (derefIdxs, [lastIdx]) = splitAt (idxCount - 1) idxs
  forM_ derefIdxs $ generateArrayDeref r
  r2 <- getFreeRegister
  generateInstrForExpr r2 e
  r3 <- getFreeRegister
  generateArrayIndex r3 lastIdx
  tell [Store CAl r2 r True (Reg r3)]
generateAssignment (PairElem p i) e = do
  r <- getRegById i
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  case p of
    Fst -> tell [Store CAl r1 r True (Imm 0)]
    Snd -> tell [Store CAl r1 r True (Imm 4)]

generateInstrForExpr :: Register -> Expr -> InstructionGenerator ()
generateInstrForExpr r (Lit l)
  = generateLiteral r l
generateInstrForExpr r (Ident id) = do
  r1 <- getRegById id
  tell [Move CAl r (Reg r1)]
generateInstrForExpr r (ArrElem id idxs) = do
  r1 <- getRegById id
  tell [Move CAl r (Reg r1)]
  forM_ idxs $ generateArrayDeref r
generateInstrForExpr r (PairElem p i) = do
  r1 <- getRegById i
  tell [Move CAl r (Reg r1)]
  case p of
    Fst -> generateAddressDerefImm r 0
    Snd -> generateAddressDerefImm r 4
generateInstrForExpr r (UnApp op e) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  case op of
    Not -> tell [Op CAl XorOp r r1 (Imm 1)]
    Neg -> tell [Negate CAl r (Reg r1)]
    Len -> tell [Load CAl r (Reg r1) True (Imm 0)]
    _   -> tell [Move CAl r (Reg r1)] -- Chr and Ord are noops in assembly
generateInstrForExpr r (BinApp op e1 e2) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e1
  r2 <- getFreeRegister
  generateInstrForExpr r2 e2
  case op of
    Add -> tell [Op CAl AddOp r r1 (Reg r2)]
    Sub -> tell [Op CAl SubOp r r1 (Reg r2)]
    Mul -> tell [Op CAl MulOp r r1 (Reg r2)]
    Div -> tell [Op CAl DivOp r r1 (Reg r2)]
    Mod -> tell [Op CAl ModOp r r1 (Reg r2)]
    And -> tell [Op CAl AndOp r r1 (Reg r2)]
    Or  -> tell [Op CAl OrOp r r1 (Reg r2)]
    Gt  -> tell [Compare CAl r1 (Reg r2), Move CGt r (Imm 1), Move CLe r (Imm 0)]
    Gte -> tell [Compare CAl r1 (Reg r2), Move CGe r (Imm 1), Move CLt r (Imm 0)]
    Lt  -> tell [Compare CAl r1 (Reg r2), Move CLt r (Imm 1), Move CGe r (Imm 0)]
    Lte -> tell [Compare CAl r1 (Reg r2), Move CLe r (Imm 1), Move CGt r (Imm 0)]
    Eq  -> tell [Compare CAl r1 (Reg r2), Move CEq r (Imm 1), Move CNe r (Imm 0)]
    NEq -> tell [Compare CAl r1 (Reg r2), Move CNe r (Imm 1), Move CEq r (Imm 0)]
generateInstrForExpr r fc@(FunCall _ _) = do
  generateInstrForFunCall fc
  tell [Move CAl r (Reg r0)]
generateInstrForExpr r (NewPair e1 e2) = do
  generateInstrForAlloc r 8
  r1 <- getFreeRegister
  generateInstrForExpr r1 e1
  tell [Store CAl r1 r True (Imm 0)]
  r2 <- getFreeRegister
  generateInstrForExpr r2 e2
  tell [Store CAl r2 r True (Imm 4)]

generateInstrForFunCall :: Expr -> InstructionGenerator ()
generateInstrForFunCall (FunCall id args) = do
  regs <- forM args $ \e -> do
    r1 <- getFreeRegister
    generateInstrForExpr r1 e
    return r1
  mapM_ (\r -> tell [Push CAl [r]]) regs
  tell [BranchLink CAl (Label id)]
  tell [Op CAl AddOp SP SP (Imm $ length args * 4)]

generateInstrForAlloc :: Register -> Int -> InstructionGenerator ()
generateInstrForAlloc r size = do
  generateInstrForFunCall (FunCall "__builtin_Alloc"
    [Lit (INT (fromIntegral size))])
  tell [Move CAl r (Reg r0)]

generateLiteral :: Register -> Literal -> InstructionGenerator ()
generateLiteral r (INT i)
  = tell [Load CAl r (Imm $ fromInteger i) True (Imm 0)]
generateLiteral r (CHAR c)
  = tell [Load CAl r (Imm $ ord c) True (Imm 0)]
generateLiteral r (BOOL b)
  = tell [Load CAl r (Imm $ bool 0 1 b) True (Imm 0)]
generateLiteral r (STR s) = do
  l <- generateLabel
  tell [Special $ StringLit l s, Load CAl r (Label l) True (Imm 0)]
generateLiteral r (ARRAY exprs) = do
  let arrLen = length exprs
  generateInstrForAlloc r ((arrLen + 1) * 4)
  r1 <- getFreeRegister
  tell [Move CAl r1 (Imm arrLen), Store CAl r1 r True (Imm 0)]
  forM_ (zip [1..] exprs) $ \(i, e) -> do
    r2 <- getFreeRegister
    generateInstrForExpr r2 e
    tell [Store CAl r2 r True (Imm (i * 4))]
generateLiteral r NULL
  = tell [Move CAl r (Imm 0)]

generateImplicitReturn :: Identifier -> InstructionGenerator ()
generateImplicitReturn "main"
  = tell [Ret (Imm 0)]
generateImplicitReturn _
  = skip

generateFunction :: Definition -> InstructionGenerator ()
generateFunction (FunDef (ident, TFun retT paramTs) stmt) = do
  resetFreeRegisters
  tell [Special $ FunctionStart ident]
  forM_ (zip [0..] paramTs) $ \(i, (id, _)) -> do
    r <- getFreeRegister
    tell [Load CAl r (Reg SP) True (Imm $ 4 + i * 4)]
    saveRegId r id
  scoped $ generateInstrForStatement stmt
  generateImplicitReturn ident

generateInstructions :: Program -> CodeGenerator [[Instruction]]
generateInstructions
  = mapM (execWriterT . generateFunction)
