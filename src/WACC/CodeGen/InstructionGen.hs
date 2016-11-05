{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.InstructionGen where

import           Data.Graph
import           Data.Array
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
  generateInstrForExpr r1 e
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
  tell [Branch CEq $ Label endLabel]
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
  tell [Special $ Ret (Reg r1)]

-- FIXME: arg needs to be checked to be in bounds of -255 < arg < 255
generateBuiltin :: BuiltinFunc -> Expr -> InstructionGenerator ()
generateBuiltin Exit e = do
  r <- getFreeRegister
  generateInstrForExpr r e
  tell [Special $ Terminate r]
generateBuiltin Free e = do
  r <- getFreeRegister
  generateInstrForExpr r e
  tell [Special $ Dealloc r]
generateBuiltin Read e
  = skip
generateBuiltin Print e
  = skip
generateBuiltin PrintLn e
  = skip

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
  forM_ derefIdxs $ \i -> generateArrayDeref r i
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
generateInstrForExpr r (FunCall id args) = do
  regs <- forM args $ \e -> do
    r1 <- getFreeRegister
    generateInstrForExpr r1 e
    return r1
  tell [Special $ FunctionCall id regs]
generateInstrForExpr r (NewPair e1 e2) = do
  tell [Special $ Alloc r 8]
  r1 <- getFreeRegister
  generateInstrForExpr r1 e1
  tell [Store CAl r1 r True (Imm 0)]
  r2 <- getFreeRegister
  generateInstrForExpr r2 e2
  tell [Store CAl r2 r True (Imm 4)]

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
  tell [Special $ Alloc r ((arrLen + 1) * 4)]
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
  = tell [Special $ Ret (Imm 0)]
generateImplicitReturn _
  = skip

generateFunction :: Definition -> InstructionGenerator ()
generateFunction (FunDef (ident, _) stmt) = do
  tell [Special $ FunctionStart ident]
  generateInstrForStatement stmt
  generateImplicitReturn ident

filterDeadVars :: RegisterAllocator ()
filterDeadVars = do
  s@RegAllocState{..} <- get
  put $ s{liveVars = filter ((instrCount >=) . liveRange) liveVars}

createEdges :: Register -> [Register] -> [Edge]
createEdges r nodes
  = (,) <$> [r] <*> nodes

addNodeToGraph :: Register -> RegisterAllocator ()
addNodeToGraph r = do
  s@RegAllocState{..} <- get
  let livingVars = map (\(LiveVar r c) -> r) liveVars
  let newEdges = edges interferenceGraph ++ createEdges r livingVars
  let newGraph = buildG (minimum (r : livingVars), maximum (r : livingVars)) newEdges
  put s{interferenceGraph = newGraph}

calcLiveRange :: Register -> [Instruction] -> Int -> Int -> Int
calcLiveRange r ((Op _ _ rd r1 (Reg r2)) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1 || r == r2 = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Op _ _ rd r1 _) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Load _ rd (Reg r1) _ (Reg r2)) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1 || r == r2 = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Load _ rd (Reg r1) _ _ ) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Store _ rd r1 _ (Reg r2)) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1 || r == r2 = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Store _ rd r1 _ _ ) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Move _ rd (Reg r1)) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Move _ rd _) : func) lastSeen line
  | r == rd            = lastSeen
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Shift _ r1 r2 _ _) : func) lastSeen line
  | r == r1 || r == r2 = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Negate _ rd (Reg r1)) : func) lastSeen line
  | r == rd            = lastSeen
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Push _ regs) : func) lastSeen line
  | r `elem` regs      = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Pop _ regs) : func) lastSeen line
  | r `elem` regs      = lastSeen
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Branch _ (Reg r1)) : func) lastSeen line
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((BranchLink _ (Reg r1)) : func) lastSeen line
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Compare _ r1 (Reg r2)) : func) lastSeen line
  | r == r1 || r == r2 = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r ((Compare _ r1 _) : func) lastSeen line
  | r == r1            = calcLiveRange r func line (line + 1)
  | otherwise          = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r (instr : func) lastSeen line
  = calcLiveRange r func lastSeen (line + 1)
calcLiveRange r [] lastSeen line
  = lastSeen

addRegToGraph :: Register -> [Instruction] -> RegisterAllocator ()
addRegToGraph reg code = do
  s@RegAllocState{..} <- get
  let line = instrCount
  let lr = LiveVar reg $ calcLiveRange reg code line line
  addNodeToGraph (varID lr)
  put s{liveVars = (lr : liveVars)}

addToGraph :: [Instruction] -> RegisterAllocator ()
addToGraph ((Op _ _ rd _ _) : func)
  = addRegToGraph rd func
addToGraph ((Load _ rd _ _ _) : func)
  = addRegToGraph rd func
addToGraph ((Store _ rd _ _ _) : func)
  = addRegToGraph rd func
addToGraph ((Move _ rd _) : func)
  = addRegToGraph rd func
addToGraph ((Pop _ regs) : func)
  = mapM_ (`addRegToGraph` func) regs
addToGraph (instr : func)
  = buildGraph func

buildGraph (instr : func) = do
  addToGraph (instr : func)
  filterDeadVars
  s@RegAllocState{..} <- get
  put $ s {instrCount = instrCount + 1}
  buildGraph func

colorGraph :: RegisterAllocator ()
colorGraph
  = return ()

allocate :: [Instruction] -> RegisterAllocator [Instruction]
allocate function
  = return []

allocateRegisters :: [Instruction] -> RegisterAllocator [Instruction]
allocateRegisters function = do
  buildGraph function
  colorGraph
  allocate function
  return function -- temporary

initRegAllocState :: RegAllocState
initRegAllocState
  = RegAllocState (array (0, -1) []) 0 []

allocateFuncRegisters :: [Instruction] -> [Instruction]
allocateFuncRegisters function
  = evalState (runRegAllocator $ allocateRegisters function) initRegAllocState

generateInstructions :: Program -> [Instruction]
generateInstructions
  = concatMap $ allocateFuncRegisters . execWriter . flip evalStateT (InstrGenState 0 0 Map.empty) . runInstrGen . generateFunction
