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

storeStruct :: Identifier -> [Declaration] -> InstructionGenerator ()
storeStruct id decls
  = modify (\s@CodeGenState{..} -> s{structDefs = Map.insert id decls structDefs})

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
getWidth TArb
  = Byte
getWidth _
  = Word

getTypeSize :: Type -> InstructionGenerator Int
getTypeSize TInt
  = pure 4
getTypeSize TChar
  = pure 1
getTypeSize TUInt8
  = pure 1
getTypeSize TUInt16
  = pure 2
getTypeSize TUInt32
  = pure 4
getTypeSize TArb
  = pure 1
getTypeSize (TStruct id) = do
  members <- fromJust . Map.lookup id <$> gets structDefs
  sum <$> mapM (getTypeSize . snd) members
getTypeSize _
  = pure 4


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
generateInstrForStatement (Loop e (Block [body@(Block _), incrementStep])) = do
  beginLabel <- generateLabel
  incrementLabel <- generateLabel
  endLabel <- generateLabel
  pushLoopLabels (incrementLabel, endLabel)
  r1 <- getFreeRegister
  tell [Special $ ScopeBegin endLabel]
  tell [Special $ LabelDecl beginLabel]
  generateInstrForExpr r1 e
  tell [Compare CAl r1 (Imm 0)]
  tell [Branch CEq $ Label endLabel]
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
  tell [Special $ ScopeBegin endLabel]
  tell [Special $ LabelDecl beginLabel]
  generateInstrForExpr r1 e
  tell [Compare CAl r1 (Imm 0)]
  tell [Branch CEq $ Label endLabel]
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
  = void $ generateInstrForExpr r0 e

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

getStructMemberInfo :: Identifier -> Identifier -> InstructionGenerator (Int, Type)
getStructMemberInfo struct member = do
  members <- fromJust . Map.lookup struct <$> gets structDefs
  let preceding = map snd $ takeWhile (not . (== member) . fst) members
  offset <- sum <$> mapM getTypeSize preceding
  return $ (offset, fromJust (lookup member members))

generateStructMemberDeref :: Register -> Expr -> InstructionGenerator Type
generateStructMemberDeref r (BinApp Member (Ident i) (Ident m)) = do
  r1 <- getRegById i
  t1 <- getTypeById i
  case r1 of
    R (-1) -> tell [ Load CAl (getWidth t1) r (Label i) True (Imm 0)
                   , Load CAl (getWidth t1) r (Reg r) True (Imm 0)
                   ]
    _ -> tell [Move CAl r (Reg r1)]
  TPtr (TStruct s) <- getTypeById i
  (offset, t) <- getStructMemberInfo s m
  generateAddressDerefImm (getWidth t) r offset
  return t
generateStructMemberDeref r (BinApp Member e (Ident m)) = do
  TPtr (TStruct s) <- generateStructMemberDeref r e
  (offset, t) <- getStructMemberInfo s m
  generateAddressDerefImm (getWidth t) r offset
  return t

generatePointerDeref :: Register -> Expr -> InstructionGenerator Type
generatePointerDeref r (UnApp Deref e) = do
  TPtr t <- generateInstrForExpr r e
  generateAddressDerefImm (getWidth t) r 0
  return t


generateAssignment :: Expr -> Expr -> InstructionGenerator ()
generateAssignment (Ident i) e = do
  r <- getRegById i
  generateAssignment' i e r
  where
    generateAssignment' :: Identifier -> Expr -> Register -> InstructionGenerator ()
    generateAssignment' i e (R (-1)) = do
      r <- getFreeRegister
      generateInstrForExpr r e
      r' <- getFreeRegister
      t <- getTypeById i
      tell [Load CAl (getWidth t) r' (Label i) True (Imm 0)]
      tell [Store CAl (getWidth t) r r' True (Imm 0)]
    generateAssignment' i e r
      = void $ generateInstrForExpr r e
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
generateAssignment (BinApp Member (Ident i) (Ident m)) e = do
  r <- getRegById i
  (TPtr (TStruct s)) <- getTypeById i
  (offset, t) <- getStructMemberInfo s m
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Store CAl (getWidth t) r1 r True (Imm offset)]
generateAssignment (BinApp Member e1 (Ident m)) e = do
  r <- getFreeRegister
  TPtr (TStruct s) <- generateStructMemberDeref r e1
  (offset, t) <- getStructMemberInfo s m
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Store CAl (getWidth t) r1 r True (Imm offset)]
generateAssignment (UnApp Deref (Ident i)) e = do
  r <- getRegById i
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  TPtr t <- getTypeById i
  tell [Store CAl (getWidth t) r1 r True (Imm 0)]
generateAssignment (UnApp Deref e1) e = do
  r <- getFreeRegister
  TPtr t <- generateInstrForExpr r e1
  r1 <- getFreeRegister
  generateInstrForExpr r1 e
  tell [Store CAl (getWidth t) r1 r True (Imm 0)]

generateInstrForExpr :: Register -> Expr -> InstructionGenerator Type
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
  return t1
generateInstrForExpr r (ArrElem id idxs) = do
  r1 <- getRegById id
  t1 <- getTypeById id
  let t1' = foldr1 (flip (.)) (map (const deconstructArrayType) idxs) t1
  tell [Move CAl r (Reg r1)]
  let idxCount = length idxs
  let (derefIdxs, [lastIdx]) = splitAt (idxCount - 1) idxs
  forM_ derefIdxs $ generateArrayDeref Word r
  generateArrayDeref (getWidth t1') r lastIdx
  return t1
generateInstrForExpr r (PairElem p i) = do
  r1 <- getRegById i
  TPair t1 t2 <- getTypeById i
  tell [Move CAl r (Reg r1)]
  case p of
    Fst -> generateAddressDerefImm (getWidth t1) r 0 >> return t1
    Snd -> generateAddressDerefImm (getWidth t2) r 4 >> return t2
generateInstrForExpr r (UnApp AddrOf (Ident i))
  = tell [Load CAl Word r (Label i) True (Imm 0)] >> TPtr <$> getTypeById i
generateInstrForExpr r e@(UnApp Deref _)
  = generatePointerDeref r e
generateInstrForExpr r (UnApp op (Ident i)) = do
  r1 <- getRegById i
  t1 <- getTypeById i
  increment <- case t1 of
                 TPtr t -> getTypeSize t
                 _      -> pure 1
  case r1 of
    (R (-1)) -> case op of
        PreInc  -> tell [ Load CAl (getWidth t1) r1 (Label i) True (Imm 0)
                        , Op CAl AddOp r1 r1 (Imm increment)
                        , Move CAl r (Reg r1)
                        ]
        PostInc -> tell [ Load CAl (getWidth t1) r1 (Label i) True (Imm 0)
                        , Move CAl r (Reg r1)
                        , Op CAl AddOp r1 r1 (Imm increment)
                        ]
        PreDec  -> tell [ Load CAl (getWidth t1) r1 (Label i) True (Imm 0)
                        , Op CAl SubOp r1 r1 (Imm increment)
                        , Move CAl r (Reg r1)
                        ]
        PostDec -> tell [ Load CAl (getWidth t1) r1 (Label i) True (Imm 0)
                        , Move CAl r (Reg r1)
                        , Op CAl SubOp r1 r1 (Imm increment)
                        ]
    _ -> case op of
        PreInc  -> tell [Op CAl AddOp r1 r1 (Imm increment), Move CAl r (Reg r1)]
        PostInc -> tell [Move CAl r (Reg r1), Op CAl AddOp r1 r1 (Imm increment)]
        PreDec  -> tell [Op CAl SubOp r1 r1 (Imm increment), Move CAl r (Reg r1)]
        PostDec -> tell [Move CAl r (Reg r1), Op CAl SubOp r1 r1 (Imm increment)]
  return t1
generateInstrForExpr r (UnApp op e) = do
  r1 <- getFreeRegister
  t1 <- generateInstrForExpr r1 e
  case op of
    Not -> tell [Op CAl XorOp r r1 (Imm 1)] >> return TBool
    Neg -> tell [Op CAl RSubOp r r1 (Imm 0)] >> return t1
    Len -> tell [Load CAl Word r (Reg r1) True (Imm 0)] >> return TInt
    Chr -> tell [Move CAl r (Reg r1)] >> return TChar -- Chr and Ord are noops in assembly
    Ord -> tell [Move CAl r (Reg r1)] >> return TInt  -- Chr and Ord are noops in assembly
    BwNot -> tell [MoveN CAl r r1] >> return t1
generateInstrForExpr r e@(BinApp Member _ _)
  = generateStructMemberDeref r e
generateInstrForExpr r (BinApp op e1 e2) = do
  r1 <- getFreeRegister
  t1 <- generateInstrForExpr r1 e1
  r2 <- getFreeRegister
  t2 <- generateInstrForExpr r2 e2
  case t1 of
    TPtr t -> if op `elem` [Add, Sub, BwAnd, BwOr, BwXor] then do
        r3 <- getFreeRegister
        s <- getTypeSize t
        tell [Move CAl r3 (Imm s), Op CAl MulOp r2 r2 (Reg r3)]
      else if op `elem` [Gt, Gte, Lt, Lte, Eq, NEq] then
        skip
      else
        fail $ "invalid pointer arithmetic operation\n" ++ show t1 ++ "\n" ++ show op
    _      -> skip
  let signedness = t1 == TInt || t2 == TInt
  case op of
    Add -> tell [Op CAl AddOp r r1 (Reg r2)] >> return t1
    Sub -> tell [Op CAl SubOp r r1 (Reg r2)] >> return t1
    Mul -> tell [Op CAl MulOp r r1 (Reg r2)] >> return t1
    Div -> tell [Op CAl (DivOp signedness) r r1 (Reg r2)] >> return t1
    Mod -> tell [Op CAl (ModOp signedness) r r1 (Reg r2)] >> return t1
    And -> tell [Op CAl AndOp r r1 (Reg r2)] >> return TBool
    Or  -> tell [Op CAl OrOp r r1 (Reg r2)] >> return TBool
    Gt  -> tell [Compare CAl r1 (Reg r2), Move CGt r (Imm 1), Move CLe r (Imm 0)] >> return TBool
    Gte -> tell [Compare CAl r1 (Reg r2), Move CGe r (Imm 1), Move CLt r (Imm 0)] >> return TBool
    Lt  -> tell [Compare CAl r1 (Reg r2), Move CLt r (Imm 1), Move CGe r (Imm 0)] >> return TBool
    Lte -> tell [Compare CAl r1 (Reg r2), Move CLe r (Imm 1), Move CGt r (Imm 0)] >> return TBool
    Eq  -> tell [Compare CAl r1 (Reg r2), Move CEq r (Imm 1), Move CNe r (Imm 0)] >> return TBool
    NEq -> tell [Compare CAl r1 (Reg r2), Move CNe r (Imm 1), Move CEq r (Imm 0)] >> return TBool
    BwAnd     -> tell [Op CAl AndOp r r1 (Reg r2)] >> return t1
    BwOr      -> tell [Op CAl OrOp r r1 (Reg r2)] >> return t1
    BwXor     -> tell [Op CAl XorOp r r1 (Reg r2)] >> return t1
    BwShiftL  -> tell [Shift CAl r r1 LSL (Reg r2)] >> return t1
    BwShiftR  -> tell [Shift CAl r r1 LSR (Reg r2)] >> return t1
generateInstrForExpr r fc@(FunCall _ _) = do
  generateInstrForFunCall fc
  tell [Move CAl r (Reg r0)]
  return TArb
--generateInstrForExpr r (NewPair e1 e2) = do
--  generateInstrForAlloc r 8
--  r1 <- getFreeRegister
--  generateInstrForExpr r1 e1
--  tell [Store CAl Word r1 r True (Imm 0)]
--  r2 <- getFreeRegister
--  generateInstrForExpr r2 e2
--  tell [Store CAl Word r2 r True (Imm 4)]
generateInstrForExpr r (SizeOf t) = do
  s <- getTypeSize t
  tell [Load CAl Word r (Imm s) True (Imm 0)]
  return TInt
generateInstrForExpr r (OffsetOf (TStruct s) m) = do
  (offset, _) <- getStructMemberInfo s m
  tell [Load CAl Word r (Imm offset) True (Imm 0)]
  return TInt

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

generateLiteral :: Register -> Literal -> InstructionGenerator Type
generateLiteral r (INT i) = do
  tell [Load CAl Word r (Imm $ fromInteger i) True (Imm 0)]
  if i < 0 then return TInt else return TUInt32
generateLiteral r (CHAR c)
  = tell [Move CAl r (Imm $ ord c)] >> return TChar
generateLiteral r (BOOL b)
  = tell [Move CAl r (Imm $ bool 0 1 b)] >> return TBool
generateLiteral r (STR s) = do
  l <- generateLabel
  tell [Special $ StringLit l s, Load CAl Word r (Label l) True (Imm 0)]
  return TString
generateLiteral r (ARRAY exprs) = do
  let arrLen = length exprs
  generateInstrForAlloc r ((arrLen + 1) * 4)
  r1 <- getFreeRegister
  tell [Move CAl r1 (Imm arrLen), Store CAl Word r1 r True (Imm 0)]
  forM_ (zip [1..] exprs) $ \(i, e) -> do
    r2 <- getFreeRegister
    generateInstrForExpr r2 e
    tell [Store CAl Word r2 r True (Imm (i * 4))]
  return $ TArray TArb
generateLiteral r NULL
  = tell [Move CAl r (Imm 0)] >> return (TPtr TArb)

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
generateDef (TypeDef id decls)
  = storeStruct id decls
generateDef (GlobalDef (id, t) e)
  = saveRegId (R $ -1) id t >> tell [Special $ GlobVarDef id e]
generateDef (ExternDef (id, TFun _ _))
  = skip
generateDef (ExternDef (id, t))
  = saveRegId (R $ -1) id t -- tell [PureAsm [".globl ", id, "\n"]]

generateInstructions :: Program -> CodeGenerator [[Instruction]]
generateInstructions
  = mapM (execWriterT . generateDef)
