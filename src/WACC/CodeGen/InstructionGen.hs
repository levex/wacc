{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.InstructionGen where

import Debug.Trace
import           Data.Bool
import           Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Maybe
import           Data.List
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
  = modify (\s@CodeGenState{..} -> s{lastRegister = 4})

getRegById :: Identifier -> InstructionGenerator Register
getRegById i
  = gets scopeId >>= getRegById' i
  where
    getRegById' :: Identifier -> Integer -> InstructionGenerator Register
    getRegById' i (-1)
      = return $ R (-1)
    getRegById' i sId = do
      table <- gets regIdsTable
      case Map.lookup (i, sId) table of
        Just (r, _) -> return r
        Nothing     -> getRegById' i (sId - 1)

getTypeById :: Identifier -> InstructionGenerator Type
getTypeById i
  = gets scopeId >>= getTypeById' i
  where
    getTypeById' :: Identifier -> Integer -> InstructionGenerator Type
    getTypeById' i (-1)
      = return TArb
    getTypeById' i sId = do
      table <- gets regIdsTable
      case Map.lookup (i, sId) table of
        Just (_, t) -> return t
        Nothing     -> getTypeById' i (sId - 1)

saveRegId :: Register -> Identifier -> Type -> InstructionGenerator ()
saveRegId r i t = do
  s@CodeGenState{..} <- get
  put s{regIdsTable = Map.insert (i, scopeId) (r, t) regIdsTable}

increaseScope :: InstructionGenerator ()
increaseScope
  = modify (\s@CodeGenState{..} -> s{scopeId = scopeId + 1})

decreaseScope :: InstructionGenerator ()
decreaseScope = modify (\s@CodeGenState{..} -> s{scopeId = scopeId - 1,
  regIdsTable = Map.filterWithKey (\(_, sId) _ -> sId < scopeId) regIdsTable})

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

pushLoopLabels :: (String, String) -> InstructionGenerator ()
pushLoopLabels ls
  = modify (\s@CodeGenState{..} -> s{loopLabels = ls:loopLabels})

popLoopLabels :: InstructionGenerator ()
popLoopLabels
  = modify (\s@CodeGenState{..} -> s{loopLabels = tail loopLabels})

deconstructArrayType :: Type -> Type
deconstructArrayType (TArray t)
  = t
deconstructArrayType TString
  = TChar

getWidth :: Type -> MemAccessType
getWidth TInt
  = Word
getWidth TChar
  = Byte
getWidth TUInt8
  = Byte
getWidth TUInt16
  = HalfWord
getWidth TUInt32
  = Word
getWidth _
  = Word

getTypeSize :: Type -> InstructionGenerator Int
getTypeSize (TPtr (TStruct s)) = do
  sd <- gets structDefs
  return $ (maximum . (map fst) . Map.elems . fromJust) (lookup s sd) + 4
getTypeSize _
  = return 4


generateInstrForStatement :: Statement -> InstructionGenerator ()
generateInstrForStatement Noop = return ()
generateInstrForStatement (Block xs) = scoped $ mapM_ generateInstrForStatement xs
generateInstrForStatement (VarDef (id, t) e) = do
  r1 <- getFreeRegister
  tell [Special $ VariableDecl id t r1]
  saveRegId r1 id t
  generateAssignment (Ident id) e
generateInstrForStatement (Ctrl c) = generateControl c
generateInstrForStatement (InlineAssembly ss) =
  tell [PureAsm ss]
generateInstrForStatement (ExternDecl id) =
  tell [PureAsm [".globl ", id, "\n"]]
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
generateInstrForStatement (Loop e (Block [body, incrementStep])) = do
  beginLabel <- generateLabel
  incrementLabel <- generateLabel
  endLabel <- generateLabel
  pushLoopLabels (incrementLabel, endLabel)
  r1 <- getFreeRegister
  tell [Special $ LabelDecl beginLabel]
  generateInstrForExpr r1 e
  tell [Compare CAl r1 (Imm 0)]
  tell [Branch CEq $ Label endLabel]
  tell [Special $ ScopeBegin endLabel]
  generateInstrForStatement body
  tell [Special $ LabelDecl incrementLabel]
  generateInstrForStatement incrementStep
  tell [Branch CAl $ Label beginLabel]
  tell [Special $ LabelDecl endLabel]
  tell [Special $ ScopeEnd endLabel]
  popLoopLabels
generateInstrForStatement (Loop e body) = do
  beginLabel <- generateLabel
  endLabel <- generateLabel
  pushLoopLabels (beginLabel, endLabel)
  r1 <- getFreeRegister
  tell [Special $ LabelDecl beginLabel]
  generateInstrForExpr r1 e
  tell [Compare CAl r1 (Imm 0)]
  tell [Branch CEq $ Label endLabel]
  tell [Special $ ScopeBegin endLabel]
  generateInstrForStatement body
  tell [Branch CAl $ Label beginLabel]
  tell [Special $ LabelDecl endLabel]
  tell [Special $ ScopeEnd endLabel]
  popLoopLabels
generateInstrForStatement (ExpStmt (BinApp Assign lhs rhs))
  = generateAssignment lhs rhs
generateInstrForStatement (ExpStmt funCall@(FunCall id args)) = do
  if "__builtin_Read_" `isPrefixOf` id then
    generateAssignment (head args) funCall
  else
    generateInstrForFunCall funCall
  when ("__builtin_" `isPrefixOf` id) $ saveBuiltinId id
generateInstrForStatement (ExpStmt e)
  = generateInstrForExpr r0 e

generateControl :: Control -> InstructionGenerator ()
generateControl (Return (Just e)) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Ret (Just $ Reg r1) [] 0]
generateControl (Return _)
  = tell [Ret Nothing [] 0]
generateControl Break
  = snd . head <$> gets loopLabels >>= \l -> tell [Branch CAl $ Label l]
generateControl Continue
  = fst . head <$> gets loopLabels >>= \l -> tell [Branch CAl $ Label l]

generateAddressDerefImm :: MemAccessType -> Register -> Int -> InstructionGenerator ()
generateAddressDerefImm w r offset
  = tell [Load CAl w r (Reg r) True (Imm offset)]

generateAddressDeref :: MemAccessType -> Register -> Register -> InstructionGenerator ()
generateAddressDeref w r offsetR
  = tell [Load CAl w r (Reg r) True (Reg offsetR)]

generateArrayIndex :: Register -> Expr -> InstructionGenerator ()
generateArrayIndex r e = do
  generateInstrForExpr r e
  tell [Op CAl AddOp r r (Imm 1), Shift CAl r r LSL (Imm 2)]

generateArrayDeref :: MemAccessType -> Register -> Expr -> InstructionGenerator ()
generateArrayDeref w r offset = do
  offsetR <- getFreeRegister
  generateArrayIndex offsetR offset
  generateAddressDeref w r offsetR

calcStructOffset :: Identifier -> Offset -> Expr -> InstructionGenerator Int
calcStructOffset s o (Ident i) = do
  sd <- gets structDefs
  let (off, _) = (fromJust $ lookup s sd) Map.! i
  return (o + off)
calcStructOffset s o (BinApp Member (Ident i) e) = do
  sd <- gets structDefs
  let (off, (TPtr (TStruct struct))) = (fromJust $ lookup s sd) Map.! i
  calcStructOffset struct (o + off) e

generateAssignment :: Expr -> Expr -> InstructionGenerator ()
generateAssignment (Ident i) e = do
  r <- getRegById i
  generateAssignment' i e r
  where
    generateAssignment' :: Identifier -> Expr -> Register -> InstructionGenerator ()
    generateAssignment' i e (R (-1)) = do
      r <- getFreeRegister
      r1 <- getFreeRegister
      generateInstrForExpr r e
      t <- getTypeById i
      tell [Load CAl Word r1 (Label i) True (Imm 0)]
      tell [Store CAl (getWidth t) r r1 True (Imm 0)]
    generateAssignment' i e r
      = generateInstrForExpr r e
generateAssignment (ArrElem i idxs) e = do
  r <- getFreeRegister
  r1 <- getRegById i
  t <- getTypeById i
  tell [Move CAl r (Reg r1)]
  let idxCount = length idxs
  let (derefIdxs, [lastIdx]) = splitAt (idxCount - 1) idxs
  forM_ derefIdxs $ generateArrayDeref Word r
  let t1 = foldr (flip (.)) id (map (const deconstructArrayType) idxs) t
  r2 <- getFreeRegister
  generateInstrForExpr r2 e
  r3 <- getFreeRegister
  generateArrayIndex r3 lastIdx
  tell [Store CAl (getWidth t1) r2 r True (Reg r3)]
generateAssignment (PairElem p i) e = do
  r <- getRegById i
  (TPair t1 t2) <- getTypeById i
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  case p of
    Fst -> tell [Store CAl (getWidth t1) r1 r True (Imm 0)]
    Snd -> tell [Store CAl (getWidth t2) r1 r True (Imm 4)]
generateAssignment (BinApp Member (Ident i) ex) e = do
  (TPtr (TStruct s)) <- getTypeById i
  offset <- calcStructOffset s 0 ex
  r <- getRegById i
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Store CAl Word r1 r True (Imm offset)]


generateInstrForExpr :: Register -> Expr -> InstructionGenerator ()
generateInstrForExpr r (Lit l)
  = generateLiteral r l
generateInstrForExpr r (Ident id) = do
  r1 <- getRegById id
  t1 <- getTypeById id
  case r1 of
    R (-1) -> do
      tell [Load CAl (getWidth t1) r (Label id) True (Imm 0)]
      tell [Load CAl (getWidth t1) r (Reg r) True (Imm 0)]
    _      -> tell [Move CAl r (Reg r1)]
generateInstrForExpr r (ArrElem id idxs) = do
  r1 <- getRegById id
  t1 <- getTypeById id
  let t1' = foldr1 (flip (.)) (map (const deconstructArrayType) idxs) t1
  tell [Move CAl r (Reg r1)]
  let idxCount = length idxs
  let (derefIdxs, [lastIdx]) = splitAt (idxCount - 1) idxs
  forM_ derefIdxs $ generateArrayDeref Word r
  generateArrayDeref (getWidth t1') r lastIdx
generateInstrForExpr r (PairElem p i) = do
  r1 <- getRegById i
  TPair t1 t2 <- getTypeById i
  tell [Move CAl r (Reg r1)]
  case p of
    Fst -> generateAddressDerefImm (getWidth t1) r 0
    Snd -> generateAddressDerefImm (getWidth t2) r 4
generateInstrForExpr r (UnApp AddrOf (Ident i))
  = tell [Load CAl Word r (Label i) True (Imm 0)]
generateInstrForExpr r (UnApp op (Ident i)) = do
  r1 <- getRegById i
  case op of
    PreInc  -> tell [Op CAl AddOp r1 r1 (Imm 1), Move CAl r (Reg r1)]
    PostInc -> tell [Move CAl r (Reg r1), Op CAl AddOp r1 r1 (Imm 1)]
    PreDec  -> tell [Op CAl SubOp r1 r1 (Imm 1), Move CAl r (Reg r1)]
    PostDec -> tell [Move CAl r (Reg r1), Op CAl SubOp r1 r1 (Imm 1)]
generateInstrForExpr r (UnApp op e) = do
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  case op of
    Not -> tell [Op CAl XorOp r r1 (Imm 1)]
    Neg -> tell [Op CAl RSubOp r r1 (Imm 0)]
    Len -> tell [Load CAl Word r (Reg r1) True (Imm 0)]
    Deref -> tell [Load CAl Word r (Reg r1) True (Imm 0)]
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
    BwAnd     -> tell [Op CAl AndOp r r1 (Reg r2)]
    BwOr      -> tell [Op CAl OrOp r r1 (Reg r2)]
    BwXor     -> tell [Op CAl XorOp r r1 (Reg r2)]
    BwShiftL  -> tell [Shift CAl r r1 LSL (Reg r2)]
    BwShiftR  -> tell [Shift CAl r r1 LSR (Reg r2)]
generateInstrForExpr r fc@(FunCall _ _) = do
  generateInstrForFunCall fc
  tell [Move CAl r (Reg r0)]
generateInstrForExpr r (NewPair e1 e2) = do
  generateInstrForAlloc r 8
  r1 <- getFreeRegister
  generateInstrForExpr r1 e1
  tell [Store CAl Word r1 r True (Imm 0)]
  r2 <- getFreeRegister
  generateInstrForExpr r2 e2
  tell [Store CAl Word r2 r True (Imm 4)]
generateInstrForExpr r (NewStruct s) = do
  sd <- gets structDefs
  generateInstrForAlloc r $ ((maximum . map fst . Map.elems . fromJust) $ lookup s sd) + 4
generateInstrForExpr r (SizeOf t) = do
  typeSize <- getTypeSize t
  tell [Load CAl Word r (Imm typeSize) True (Imm 0)]

generateInstrForFunCall :: Expr -> InstructionGenerator ()
generateInstrForFunCall (FunCall id args) = do
  regs <- forM args $ \e -> do
    r1 <- getFreeRegister
    generateInstrForExpr r1 e
    return r1
  mapM_ (\r -> tell [Push CAl [r]]) (reverse regs)
  tell [BranchLink CAl (Label id)]
  when (not . null $ args) $ tell [Op CAl AddOp SP SP (Imm $ length args * 4)]

generateInstrForAlloc :: Register -> Int -> InstructionGenerator ()
generateInstrForAlloc r size = do
  saveBuiltinId "__builtin_Alloc"
  generateInstrForFunCall (FunCall "__builtin_Alloc"
    [Lit (INT (fromIntegral size))])
  tell [Move CAl r (Reg r0)]

generateLiteral :: Register -> Literal -> InstructionGenerator ()
generateLiteral r (INT i)
  = tell [Load CAl Word r (Imm $ fromInteger i) True (Imm 0)]
generateLiteral r (CHAR c)
  = tell [Move CAl r (Imm $ ord c)]
generateLiteral r (BOOL b)
  = tell [Move CAl r (Imm $ bool 0 1 b)]
generateLiteral r (STR s) = do
  l <- generateLabel
  tell [Special $ StringLit l s, Load CAl Word r (Label l) True (Imm 0)]
generateLiteral r (ARRAY exprs) = do
  let arrLen = length exprs
  generateInstrForAlloc r ((arrLen + 1) * 4)
  r1 <- getFreeRegister
  tell [Move CAl r1 (Imm arrLen), Store CAl Word r1 r True (Imm 0)]
  forM_ (zip [1..] exprs) $ \(i, e) -> do
    r2 <- getFreeRegister
    generateInstrForExpr r2 e
    tell [Store CAl Word r2 r True (Imm (i * 4))]
generateLiteral r NULL
  = tell [Move CAl r (Imm 0)]

generateDef :: Definition -> InstructionGenerator ()
generateDef (FunDef (ident, TFun retT paramTs) stmt) = do
  resetFreeRegisters
  tell [Special $ FunctionStart ident [] 0]
  forM_ (zip [0..] paramTs) $ \(i, (id, t)) -> do
    r <- getFreeRegister
    tell [Load CAl Word r (Reg SP) True (Imm $ 8 + i * 4)]
    saveRegId r id t
  scoped $ generateInstrForStatement stmt
  when (retT == TArb) $ tell [Ret Nothing [] 0]
generateDef (TypeDef _ _)
  = pure ()
generateDef (GlobalDef (id, t) e)
  = tell [Special $ GlobVarDef id e]

generateInstructions :: Program -> CodeGenerator [[Instruction]]
generateInstructions
  = mapM (execWriterT . generateDef)
