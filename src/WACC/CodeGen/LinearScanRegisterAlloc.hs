{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE BangPatterns #-}
module WACC.CodeGen.LinearScanRegisterAlloc where

import           Data.Graph
import           Data.Array
import           Data.Either
import           Data.Tree
import           Data.List
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Debug.Trace
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Trans.Either
import           WACC.Parser.Types
import           WACC.CodeGen.Types

--  http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

availableRegisters = map R [4..11]

data LiveRange = LiveRange
  { registerOld  :: Register
  , registerNew  :: Maybe Register
  , startLine    :: Int
  , endLine      :: Int
  , location     :: Maybe Int
  } deriving (Eq, Show)

selReg :: LiveRange -> Register
selReg lr = case location lr of
  Nothing -> case registerNew lr of
    Nothing -> registerOld lr
    Just x  -> x
  Just _ -> error " uh"

data LSRAState = LSRAState
  { lranges          :: [LiveRange]
  , instructions     :: [Instruction]
  , active           :: [LiveRange]
  , freePool         :: [Register]
  , finalAllocations :: [LiveRange]
  , lastStack        :: Int
  , spillage         :: [(Int, Spill)]
  } 

initialLSRAState :: [Instruction] -> LSRAState
initialLSRAState p = LSRAState
  { lranges = []
  , instructions = p
  , active = []
  , freePool = availableRegisters
  , finalAllocations = []
  , lastStack = 0
  , spillage = []
  }

newtype LSRA a = LSRA { unLSRA :: State LSRAState a }
                  deriving (Functor, Applicative, Monad, MonadFix,
                            MonadState LSRAState)

data RegAction = {- RReadNeedBe | -} RRead | RWrite | RIgnore

data Spill = SpillStart Int Int Register Int Register | SpillEnd
              deriving (Eq, Show)

getNewStackLocation :: LSRA Int
getNewStackLocation = do
  st@LSRAState{..} <- get
  put st{lastStack = lastStack + 4}
  return lastStack

analyzeAccess :: Register -> Instruction -> RegAction
{- analyzeAccess r (Op _ AddOp SP SP _)
  = RReadNeedBe -}
analyzeAccess r (Op _ _ rd r1 (Reg r2))
  | r == rd = RWrite
  | r == r1 || r == r2 = RRead
  | otherwise = RIgnore

analyzeAccess r (Op _ _ rd r1 _)
  | r == rd            = RWrite
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Load _ _ rd (Reg r1) _ (Reg r2))
  | r == rd            = RWrite
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Load _ _ rd (Reg r1) _ _)
  | r == rd            = RWrite
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Load _ _ rd _ _ _)
  | r == rd            = RWrite
  | otherwise          = RIgnore

analyzeAccess r (Store _ _ rd r1 _ (Reg r2))
  | r == rd            = RRead
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Store _ _ rd r1 _ _ )
  | r == rd            = RRead
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Move _ rd (Reg r1))
  | r == rd            = RWrite
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Move _ rd _)
  | r == rd            = RWrite
  | otherwise          = RIgnore

analyzeAccess r (Shift _ rd r1 _ _)
  | r == rd            = RWrite
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Push _ regs)
  | r `elem` regs      = RRead
  | otherwise          = RIgnore

analyzeAccess r (Pop _ regs)
  | r `elem` regs      = RWrite
  | otherwise          = RIgnore

analyzeAccess r (Branch _ (Reg r1))
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (BranchLink _ (Reg r1))
  | r == r1            = RRead
  | otherwise          = RRead

analyzeAccess r (Compare _ r1 (Reg r2))
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Compare _ r1 _)
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Ret (Reg r1) _ _)
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess _ _ = RIgnore

------------------------------------------------

liveRangeMap :: LiveRange -> (Register, LiveRange)
liveRangeMap = (,) =<< registerOld

replace' :: [LiveRange] -> Register -> Register
replace' rs r
  = case lookup r (liveRangeMap <$> rs) of
      Nothing -> r
      Just lr -> if isNothing $ location lr then fromJust $ registerNew lr
                  else r
                  -- else (fromJust $ registerNew lr)
                    -- FOR NOW, leave this as is

addReplacements _ _ = []
{-
addReplacements :: Instruction -> [LiveRange] -> [Instruction]
addReplacements ins rs = concatMap (\r -> case lookup r (liveRangeMap <$> rs) of
    Nothing -> []
    Just lr -> case location lr of
      Nothing -> []
      Just loc -> [Load CAl Word (fromJust $ registerNew lr) (Reg SP) True (Imm loc)])
          (nub $ collectRegisters ins)
-}

replaceRegister :: [LiveRange] -> Instruction  -> [Instruction]
replaceRegister rs ins@(Op a b rd r1 (Reg r2))
  = addReplacements ins rs ++ [Op a b (replace' rs rd) (replace' rs r1) (Reg (replace' rs r2))]

replaceRegister rs ins@(Op a b rd r1 c)
  = addReplacements ins rs ++ [Op a b (replace' rs rd) (replace' rs r1) c]

replaceRegister rs ins@(Load a b rd (Reg r1) c (Reg r2))
  = addReplacements ins rs ++ [Load a b (replace' rs rd) (Reg (replace' rs r1)) c (Reg (replace' rs r2))]

replaceRegister rs ins@(Load a b rd (Reg r1) c d)
  = addReplacements ins rs ++ [Load a b (replace' rs rd) (Reg (replace' rs r1)) c d]

replaceRegister rs ins@(Load a b rd c d e)
  = addReplacements ins rs ++ [Load a b (replace' rs rd) c d e]

replaceRegister rs ins@(Store a b rd r1 c (Reg r2))
  = addReplacements ins rs ++ [Store a b (replace' rs rd) (replace' rs r1) c (Reg (replace' rs r2))]

replaceRegister rs ins@(Store a b rd r1 c d)
  = addReplacements ins rs ++ [Store a b (replace' rs rd) (replace' rs r1) c d]

replaceRegister rs ins@(Move a rd (Reg r1))
  = addReplacements ins rs ++ [Move a (replace' rs rd) (Reg (replace' rs r1))]

replaceRegister rs ins@(Move a rd b)
  = addReplacements ins rs ++ [Move a (replace' rs rd) b]

replaceRegister rs ins@(Shift a r1 r2 b c)
  = addReplacements ins rs ++ [Shift a (replace' rs r1) (replace' rs r2) b c]

replaceRegister rs ins@(Push a regs)
  = addReplacements ins rs ++ [Push a (map (replace' rs) regs)]

replaceRegister rs ins@(Pop a regs)
  = addReplacements ins rs ++ [Pop a (map (replace' rs) regs)]

replaceRegister rs ins@(Branch a (Reg r1))
  = addReplacements ins rs ++ [Branch a (Reg (replace' rs r1))]

replaceRegister rs ins@(BranchLink a (Reg r1))
  = addReplacements ins rs ++ [BranchLink a (Reg (replace' rs r1))]

replaceRegister rs ins@(Compare a r1 (Reg r2))
  = addReplacements ins rs ++ [Compare a (replace' rs r1) (Reg (replace' rs r2))]

replaceRegister rs ins@(Compare a r1 b)
  = addReplacements ins rs ++ [Compare a (replace' rs r1) b]

replaceRegister rs ins@(Ret (Reg r) a b)
  = addReplacements ins rs ++ [Ret (Reg (replace' rs r)) a b]

replaceRegister rs ins@(Special (VariableDecl a b r))
  = addReplacements ins rs ++ [Special (VariableDecl a b (replace' rs r))]

replaceRegister _ i = [i]

----------

collectRegisters :: Instruction -> [Register]
collectRegisters (Op a b rd tr1 (Reg tr2))
  = [rd, tr1, tr2]

collectRegisters (Op a b rd tr1 c)
  = [rd, tr1]

collectRegisters (Load a b rd (Reg tr1) c (Reg tr2))
  = [rd, tr1, tr2]

collectRegisters (Load a b rd (Reg tr1) c d)
  = [rd, tr1]

collectRegisters (Load a b rd c d e)
  = [rd]

collectRegisters (Store a b rd tr1 c (Reg tr2))
  = [rd, tr1, tr2]

collectRegisters (Store a b rd tr1 c d)
  = [rd, tr1]

collectRegisters (Move a rd (Reg tr1))
  = [rd, tr1]

collectRegisters (Move a rd b)
  = [rd]

collectRegisters (Shift a tr1 tr2 b c)
  = [tr1, tr2]

collectRegisters (Push a regs)
  = regs

collectRegisters (Pop a regs)
  = regs

collectRegisters (Branch a (Reg tr1))
  = [tr1]

collectRegisters (BranchLink a (Reg tr1))
  = [tr1]

collectRegisters (Compare a tr1 (Reg tr2))
  = [tr1, tr2]

collectRegisters (Compare a tr1 b)
  = [tr1]

collectRegisters (Ret (Reg r) a b)
  = [r]

collectRegisters (Special (VariableDecl a b r))
  = [r]

collectRegisters _ = []

----------

calcLiveRange :: Register -> Int -> Instruction -> LSRA ()
calcLiveRange r@(R _) line ins@(analyzeAccess r -> RWrite) = do
  st@LSRAState{..} <- get
  let mran = lookup r (liveRangeMap <$> lranges)
  case mran of
    Nothing ->
      put st{lranges = LiveRange { startLine = line, registerOld = r, registerNew = Nothing
                , endLine = line, location = Nothing
                } : lranges}
    Just range -> put st{lranges = range{endLine = line} : delete range lranges}
calcLiveRange r@(R _) line ins@(analyzeAccess r -> RRead) = do
  st@LSRAState{..} <- get
  let mran = lookup r (liveRangeMap <$> lranges)
  case mran of
    Nothing -> error "invalid register access"
    Just range -> put st{lranges = range{endLine = line} : delete range lranges}
calcLiveRange _ _ _ = return ()

extendLiveRange :: Register -> [Instruction] -> LSRA ()
extendLiveRange r ins = do
  LSRAState{..} <- get
  let range = fromJust $ lookup r (liveRangeMap <$> lranges)
  extendLiveRange' (startLine range) (drop (startLine range) ins) Nothing range
  where
    extendLiveRange' :: Int -> [Instruction] -> Maybe Identifier -> LiveRange -> LSRA ()
    extendLiveRange' line ((Special (ScopeBegin s)):ins) Nothing range
      = extendLiveRange' (line + 1) ins (Just s) range
    extendLiveRange' line ((Special (ScopeEnd s)):ins) (Just s') range
      | s == s' && endLine range < line
          = modify (\st@LSRAState{..} ->
              st{lranges = range{endLine = line} : delete range lranges})
      | otherwise = extendLiveRange' (line + 1) ins Nothing range
    extendLiveRange' line (_:ins) s range
      | line <= endLine range || isJust s
          = extendLiveRange' (line + 1) ins s range
      | otherwise             = return ()
    extendLiveRange' line [] s range
      = return ()

allocateLSRA :: LSRA ()
allocateLSRA = mdo
  -- calculate live ranges
  ins <- gets instructions
  let usedRegs = nub $ [r | r@(R _) <- concatMap collectRegisters ins, r /= r0]
  --traceShowM ("--------> USED REGS: " ++ show usedRegs)
  forM_ usedRegs $ \r -> do
    forM_ (zip [0..] ins) $ \(i, instr) -> calcLiveRange r i instr
    extendLiveRange r ins
  -- start LSRA
  lr <- sortOn startLine <$> gets lranges
  forM_ lr $ \i -> do
    expireOldIntervals i
    len <- length <$> gets active
    -- traceShowM len
    if len >= length availableRegisters then
      spillAtInterval i
    else do
      st@LSRAState{..} <- get
      let rt = head freePool
      let newi = i{registerNew = Just rt}
      put st{ freePool = freePool \\ [rt]
            , finalAllocations = newi : finalAllocations  -- r got assigned to rt
            , active = nub $ sortOn endLine (newi : active)  -- i is now active
            }
      
  --traceM $ show lr

expireOldIntervals :: LiveRange -> LSRA ()
expireOldIntervals i = do
  beginActives <- gets active
  void $ runEitherT (forM_ (sortOn endLine beginActives) $ \j -> do
    if endLine j >= startLine i then do
      left () -- exit from the loop
    else do
      st2 <- get
      let acts = sortOn endLine (active st2)
      --traceShowM ("lr " ++ show i ++ " adding " ++ (show $ selReg j) ++ " to freepool")
      put st2{active = nub $ (acts \\ [j]),     -- remove interval j from active intervals
            freePool = selReg j : freePool st2}) -- remove reg j from the freepool

spillAtInterval :: LiveRange -> LSRA ()
spillAtInterval i = do
  st <- get
  let spill = last (sortOn endLine (active st))
  stackloc <- getNewStackLocation
  st2@LSRAState{..} <- get
  if (endLine spill > endLine i) then do
    let newi = i{registerNew = Just $ selReg spill}
    put st2{ finalAllocations = newi : spill{location = Just stackloc} : finalAllocations
           , spillage
            = (startLine i,
                SpillStart  (max (startLine spill) (startLine i))
                            (min (endLine i) (endLine spill))
                            (selReg spill)
                            stackloc
                            (registerOld spill)) : spillage
           , active = nub $ sortOn endLine (newi : (active \\ [spill]))
           }
  else
    put st2{finalAllocations = i{location = Just stackloc} : finalAllocations }

testInstructions :: [Instruction]
testInstructions =
  [ Load CAl Word (R 4) (Imm 0) True (Imm 0) -- l0  -- ldr r4, #0
  , Load CAl Word (R 5) (Imm 1) True (Imm 0) -- l1  -- ldr r5, #1
  , Load CAl Word (R 6) (Imm 2) True (Imm 0) -- l2  -- ldr r6, #2
  , Load CAl Word (R 7) (Imm 3) True (Imm 0) -- l2  -- ldr r6, #2
  , Op   CAl AddOp (R 8) (R 4) (Reg (R 5)) -- l3  -- add r7, r4, r5
  , Op   CAl AddOp (R 9) (R 8) (Reg (R 6)) -- l4  -- add r8, r7, r6
  , Load CAl Word (R 10) (Imm 5) True (Imm 0)
  , Op   CAl AddOp (R 11) (R 10) (Reg (R 4))
  , Op   CAl AddOp (R 12) (R 5) (Reg (R 5))
  , Op   CAl AddOp (R 13) (R 4) (Reg (R 6))
  , Op   CAl AddOp (R 14) (R 7) (Reg (R 6))
  ]

runLSRA = allocateFuncRegisters

thsnd (_, a, _) = a

instrUsesRegister :: Instruction -> Register -> Bool
instrUsesRegister instr reg
  = reg `elem` (nub $ collectRegisters instr)

spillOldReg   (SpillStart _ _ _ _ o) = o
spillNewReg   (SpillStart _ _ n _ _) = n
spillEnd      (SpillStart _ e _ _ _) = e
spillStart    (SpillStart s _ _ _ _) = s
spillLocation (SpillStart _ _ _ l _) = l

spillNewReg2 allocs (SpillStart _ _ _ _ o)
  = case lookup o m of
      Just lr -> fromJust $ registerNew lr
      Nothing -> error "screw this"
  where
    m = liveRangeMap <$> allocs

allocateFuncRegisters :: [Instruction] -> [Instruction]
allocateFuncRegisters p
  | null (concatMap collectRegisters p) = p
  | otherwise
    = reverse . snd $ foldl f (0, []) (concatMap (replaceRegister allocs) p)
  where
    final = (execState . unLSRA) allocateLSRA (initialLSRAState p)
    instrs = instructions final
    usedRegs = sort . nub . catMaybes . map registerNew $ allocs
    spills = spillage final
    allocs = finalAllocations final
    locRemAllocs = map (\lr -> if (isNothing $ location lr) then lr{location = Just 1337} else lr{location = Nothing}) allocs
    f (i, acc) instr
      = case lookup i spills of
          --Nothing -> (i + 1, fixStackAccesses instr : acc, activeSpills)
          Nothing ->
            if or (map (instrUsesRegister (p !! i) . spillOldReg) (snd <$> spills)) then
              -- this instruction uses a register that has been spilled
              let
                spilledRegisters = filter (\x -> instrUsesRegister (p !! i) (spillOldReg x)) (snd <$> spills)
                instructionsToPush
                  = map (\sp@(SpillStart start end nre nloc ore) ->
                            (if i > start then Load CAl Word nre (Reg $ R 12) True (Imm nloc)
                              else Special Empty)) spilledRegisters
              in
                (i + 1, ((fixStackAccesses <$> (replaceRegister locRemAllocs instr)) ++ instructionsToPush) ++ acc)
            else
              (i + 1, fixStackAccesses <$> (replaceRegister locRemAllocs instr) ++ acc)
          Just spilly@(SpillStart startLocation endLocation spilledReg spillLocation oldReg) ->
            (i + 1,
             fixStackAccesses instr :
               Store CAl Word spilledReg (R 12) True (Imm spillLocation) : acc
             )
    fixStackAccesses (Special (FunctionStart s _ _))
      = Special (FunctionStart s usedRegs (lastStack final))
    fixStackAccesses (Load c m r (Reg SP) plus (Imm i))
      = Load c m r (Reg SP) plus (Imm (i + 16 + (length usedRegs * 4)))
    fixStackAccesses (Store c m r SP plus (Imm i))
      = Store c m r SP plus (Imm (i + 16 + (length usedRegs * 4)))
    fixStackAccesses (Ret op _ _)
      = Ret op usedRegs (lastStack final)
    fixStackAccesses i
      = i
