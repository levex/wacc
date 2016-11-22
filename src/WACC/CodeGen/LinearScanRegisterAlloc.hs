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

availableRegisters = map R [4..12]

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
  , spillage         :: [(Int, (Condition -> [Register] -> Instruction, Register))]
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

data RegAction = RRead | RWrite | RIgnore

getNewStackLocation :: LSRA Int
getNewStackLocation = do
  st@LSRAState{..} <- get
  put st{lastStack = lastStack + 1}
  return lastStack

analyzeAccess :: Register -> Instruction -> RegAction
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
  | r == rd            = RWrite
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Store _ _ rd r1 _ _ )
  | r == rd            = RWrite
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

analyzeAccess r (Negate _ rd (Reg r1))
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
  | otherwise          = RIgnore

analyzeAccess r (Compare _ r1 (Reg r2))
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Compare _ r1 _)
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Ret (Reg r1))
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

replaceRegister :: [LiveRange] -> Instruction  -> Instruction
replaceRegister rs (Op a b rd r1 (Reg r2))
  = Op a b (replace' rs rd) (replace' rs r1) (Reg (replace' rs r2))

replaceRegister rs (Op a b rd r1 c)
  = Op a b (replace' rs rd) (replace' rs r1) c

replaceRegister rs (Load a b rd (Reg r1) c (Reg r2))
  = Load a b (replace' rs rd) (Reg (replace' rs r1)) c (Reg (replace' rs r2))

replaceRegister rs (Load a b rd (Reg r1) c d)
  = Load a b (replace' rs rd) (Reg (replace' rs r1)) c d

replaceRegister rs (Load a b rd c d e)
  = Load a b (replace' rs rd) c d e

replaceRegister rs (Store a b rd r1 c (Reg r2))
  = Store a b (replace' rs rd) (replace' rs r1) c (Reg (replace' rs r2))

replaceRegister rs (Store a b rd r1 c d)
  = Store a b (replace' rs rd) (replace' rs r1) c d

replaceRegister rs (Move a rd (Reg r1))
  = Move a (replace' rs rd) (Reg (replace' rs r1))

replaceRegister rs (Move a rd b)
  = Move a (replace' rs rd) b

replaceRegister rs (Shift a r1 r2 b c)
  = Shift a (replace' rs r1) (replace' rs r2) b c

replaceRegister rs (Negate a rd (Reg r1))
  = Negate a (replace' rs rd) (Reg (replace' rs r1))

replaceRegister rs (Push a regs)
  = Push a (map (replace' rs) regs)

replaceRegister rs (Pop a regs)
  = Pop a (map (replace' rs) regs)

replaceRegister rs (Branch a (Reg r1))
  = Branch a (Reg (replace' rs r1))

replaceRegister rs (BranchLink a (Reg r1))
  = BranchLink a (Reg (replace' rs r1))

replaceRegister rs (Compare a r1 (Reg r2))
  = Compare a (replace' rs r1) (Reg (replace' rs r2))

replaceRegister rs (Compare a r1 b)
  = Compare a (replace' rs r1) b

replaceRegister rs (Ret (Reg r))
  = Ret (Reg (replace' rs r))

replaceRegister _ i = i

----------

collectRegisters :: Instruction -> [Register]
collectRegisters (Op a b rd r1 (Reg r2))
  = [rd, r1, r2]

collectRegisters (Op a b rd r1 c)
  = [rd, r1]

collectRegisters (Load a b rd (Reg r1) c (Reg r2))
  = [rd, r1, r2]

collectRegisters (Load a b rd (Reg r1) c d)
  = [rd, r1]

collectRegisters (Load a b rd c d e)
  = [rd]

collectRegisters (Store a b rd r1 c (Reg r2))
  = [rd, r1, r2]

collectRegisters (Store a b rd r1 c d)
  = [rd, r1]

collectRegisters (Move a rd (Reg r1))
  = [rd, r1]

collectRegisters (Move a rd b)
  = [rd]

collectRegisters (Shift a r1 r2 b c)
  = [r1, r2]

collectRegisters (Negate a rd (Reg r1))
  = [rd, r1]

collectRegisters (Push a regs)
  = regs

collectRegisters (Pop a regs)
  = regs

collectRegisters (Branch a (Reg r1))
  = [r1]

collectRegisters (BranchLink a (Reg r1))
  = [r1]

collectRegisters (Compare a r1 (Reg r2))
  = [r1, r2]

collectRegisters (Compare a r1 b)
  = [r1]

collectRegisters (Ret (Reg r))
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
  st@LSRAState{..} <- get
  let range = fromJust $ lookup r (liveRangeMap <$> lranges)
  extendLiveRange' (startLine range) (drop (startLine range) ins) Nothing
  where
    extendLiveRange' :: Int -> [Instruction] -> Maybe Identifier -> LSRA ()
    extendLiveRange' line ((Special (ScopeBegin s)):ins) Nothing
      = extendLiveRange' (line + 1) ins (Just s)
    extendLiveRange' line ((Special (ScopeEnd s)):ins) (Just s')
      | s == s' = modify (\st@LSRAState{..} ->
        let range = fromJust $ lookup r (liveRangeMap <$> lranges)
        in st{lranges = range{endLine = line} : delete range lranges})
    extendLiveRange' line (_:ins) s
      = extendLiveRange' (line + 1) ins s
    extendLiveRange' line [] s
      = return ()

allocateLSRA :: LSRA ()
allocateLSRA = mdo
  -- calculate live ranges
  ins <- gets instructions
  let usedRegs = [r | r@(R _) <- concatMap collectRegisters ins, r /= r0]
  forM_ usedRegs $ \r -> do
    forM_ (zip [0..] ins) $ \(i, instr) -> calcLiveRange r i instr
    extendLiveRange r ins
  -- start LSRA
  lr <- sortOn startLine <$> gets lranges
  forM_ lr $ \i -> do
    expireOldIntervals i
    len <- length <$> gets active
    -- traceShowM len
    if len == length availableRegisters then
      spillAtInterval i
    else do
      st@LSRAState{..} <- get
      let rt = head freePool
      traceShowM ("chose: " ++ show rt)
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
  st@LSRAState{..} <- get
  let spill = last (sortOn endLine active)
  stack <- getNewStackLocation
  if (endLine spill > endLine i) then do
    let newi = i{registerNew = Just $ selReg spill}
    put st{ finalAllocations = newi : spill{location = Just stack} : finalAllocations
          , spillage = (startLine i, (Push, selReg spill))
                        : (endLine spill - 1, (Pop, selReg spill))
                          : spillage
          , active = nub $ sortOn endLine (newi : (active \\ [spill]))
          }
  else
    put st{finalAllocations = i{location = Just stack} : finalAllocations }

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

allocateFuncRegisters :: [Instruction] -> [Instruction]
allocateFuncRegisters p
  | null (concatMap collectRegisters p) = p
  | otherwise = reverse . snd $ foldl f (0, []) (map (replaceRegister allocs) p)
  where
    final = (execState . unLSRA) allocateLSRA (initialLSRAState p)
    instrs = instructions final
    usedRegs = sort . nub . catMaybes . map registerNew $ allocs
    spills = spillage final
    allocs = finalAllocations final
    f (i, acc) instr
      = case lookup i spills of
          Nothing -> (i + 1, fixStackAccesses instr ++ acc)
          Just (fi, reg) ->
            (i + 1, fixStackAccesses instr ++ (fi CAl [reg] : acc))
    fixStackAccesses i@(Special (FunctionStart _))
      = [Push CAl usedRegs, i] -- reverse due to foldl
    fixStackAccesses (Load c m r (Reg SP) plus (Imm i))
      = [Load c m r (Reg SP) plus (Imm (i + (length usedRegs * 4)))]
    fixStackAccesses (Store c m r SP plus (Imm i))
      = [Store c m r SP plus (Imm (i + (length usedRegs * 4)))]
    fixStackAccesses i@(Ret _)
      = [i, Pop CAl usedRegs] -- reverse due to foldl
    fixStackAccesses i
      = [i]
