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
  , spillage         :: [(Register, Spill)]
  } 

data Spill = Spill
  { spillRegisterOld :: Register
  , spillLocation    :: Int
  , spilledLines     :: [Int]
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
                  deriving (Show, Eq)

getNewStackLocation :: LSRA Int
getNewStackLocation = do
  st@LSRAState{..} <- get
  put st{lastStack = lastStack + 4}
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

analyzeAccess r (MoveN _ rd r1)
  | r == rd            = RWrite
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Shift _ rd r1 _ (Reg r2))
  | r == rd            = RWrite
  | r == r1 || r == r2 = RRead
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
  | otherwise          = RIgnore

analyzeAccess r (Compare _ r1 (Reg r2))
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Compare _ r1 _)
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Ret (Just (Reg r1)) _ _)
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

replaceRegister rs (MoveN a rd r1)
  = MoveN a (replace' rs rd) (replace' rs r1)

replaceRegister rs (Shift a r1 r2 b (Reg r3))
  = Shift a (replace' rs r1) (replace' rs r2) b (Reg (replace' rs r3))

replaceRegister rs (Shift a r1 r2 b c)
  = Shift a (replace' rs r1) (replace' rs r2) b c

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

replaceRegister rs (Ret (Just (Reg r)) a b)
  = Ret (Just (Reg (replace' rs r))) a b

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

collectRegisters (MoveN a rd r1)
  = [rd, r1]

collectRegisters (Shift a r1 r2 b (Reg r3))
  = [r1, r2, r3]

collectRegisters (Shift a r1 r2 b c)
  = [r1, r2]

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

collectRegisters (Ret (Just (Reg r)) a b)
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
  case lookup r (liveRangeMap <$> lranges) of
    Just range -> extendLiveRange' (startLine range) (drop (startLine range) ins) Nothing range
    Nothing -> return ()
  where
    extendLiveRange' :: Int -> [Instruction] -> Maybe Identifier -> LiveRange -> LSRA ()
    extendLiveRange' line ((Special (ScopeBegin s)):ins) Nothing range
      = extendLiveRange' (line + 1) ins (Just s) range
    extendLiveRange' line ((Special (ScopeEnd s)):ins) (Just s') range
      | s == s' && endLine range < line
          = modify (\st@LSRAState{..} ->
              st{lranges = range{endLine = line} : delete range lranges})
      | otherwise = extendLiveRange' (line + 1) ins (Just s') range
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
          newi = i{registerNew = Just rt}
      put st{ freePool = freePool \\ [rt]
            , finalAllocations = newi : (delete i finalAllocations)  -- r got assigned to rt
            , active = nub $ sortOn endLine (newi : delete i active)  -- i is now active
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
            freePool = nub $ ((selReg j) : freePool st2)}) -- remove reg j from the freepool

spillAtInterval :: LiveRange -> LSRA ()
spillAtInterval i = do
  spill <- last . (sortOn endLine) <$> gets active
  stack <- getNewStackLocation
  st@LSRAState{..} <- get
  --if (endLine spill > endLine i) then do
  let newi = i{registerNew = Just $ selReg spill}
  put st{ finalAllocations = newi : spill{location = Just stack} : finalAllocations
        , spillage
          = (registerOld spill,
              Spill{spillRegisterOld = registerOld spill,
                    spilledLines = [startLine i .. endLine i],
                    spillLocation = stack}
            ) : spillage
        , active = nub $ sortOn endLine (newi : (active \\ [spill]))
        }
  {-else do
    put st{ finalAllocations = i{registerNew = Just $ R 11, location = Just stack} : finalAllocations
          , spillage
            = (registerOld spill,
                Spill{spillRegisterOld = registerOld spill,
                      spilledLines = [startLine i .. endLine i],
                      spillLocation = stack}
              ) : spillage
           } -}
              

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

allocateFuncRegisters :: [Instruction] -> [Instruction]
allocateFuncRegisters p
  | null (concatMap collectRegisters p) = p
  | otherwise = reverse . thsnd $ foldl f (0, [], []) (map (replaceRegister allocs) p)
  where
    final = (execState . unLSRA) allocateLSRA (initialLSRAState p)
    instrs = instructions final
    usedRegs = sort . nub . catMaybes . map registerNew $ allocs
    spills = spillage final
    allocs = finalAllocations final
    totalStack = lastStack final
    f (i, acc, contested) instr
      = let
          cLookup :: [(Register, Spill)] -> Register -> [Spill]
          cLookup rs r = map snd $ filter (\(a, b) -> a == r) rs

          oldRegistersInInstr
            = collectRegisters (p !! i)
          oldRegistersWritten
            = filter (\r -> analyzeAccess r (p !! i) == RWrite) oldRegistersInInstr
          oldRegistersRead
            = filter (\r -> analyzeAccess r (p !! i) == RRead) oldRegistersInInstr
          spilledOldRegistersWritten
            = filter (\r -> (not . null) $ cLookup spills r) oldRegistersWritten
          spilledOldRegistersRead
            = filter (\r -> (not . null) $ cLookup spills r) oldRegistersRead

          ourContestedRegisters
            = (spilledOldRegistersWritten `union` spilledOldRegistersRead) --`intersect` contested

          rAllocs
            = map (\a -> if isNothing $ location a
                          then a{location = Just 1337}
                          else a{location = Nothing})
                  allocs

          replacedInstr = replaceRegister rAllocs instr

          newRegister r = replace' rAllocs r
          theLocation r = spillLocation . fromJust $ lookup r spills
          isCurrentLineLiveOfReg r
            = if isNothing $ lookup r ll then False else i `elem` lines
            where rlr = fromJust $ lookup r ll
                  lines = [startLine rlr..endLine rlr]
                  ll = liveRangeMap <$> (lranges final)

          storeInstructions
            = map (\r -> if isNothing $ lookup r spills then Special Empty else
                Store CAl Word (newRegister r) (R 12) True (Imm $ theLocation r)
              ) spilledOldRegistersWritten

          readInstructions
            = map (\r -> if isNothing $ lookup r spills then Special Empty else
                Load CAl Word (newRegister r) (Reg $ R 12) True (Imm $ theLocation r)
              ) spilledOldRegistersRead

          pushInstructions
            = map (\r ->
                if isCurrentLineLiveOfReg (replace' rAllocs r) then
                  Push CAl [replace' rAllocs r]
                else Special Empty
              ) (spilledOldRegistersRead `union` spilledOldRegistersWritten)

          popInstructions
            = reverse $ map (\r ->
                if isCurrentLineLiveOfReg (replace' rAllocs r) then
                  Pop CAl [replace' rAllocs r]
                else Special Empty
              ) (spilledOldRegistersRead `union` spilledOldRegistersWritten)

          orig =
            (i + 1,
              popInstructions ++ storeInstructions ++ 
                  (fixStackAccesses replacedInstr
              : readInstructions) ++ pushInstructions ++
                acc, contested)
        in
          case replacedInstr of
            Move CAl a (Reg b) -> if a == b
                  then (i + 1, 
                      storeInstructions ++ (fixStackAccesses replacedInstr : readInstructions) ++ acc, contested)
                                else orig
            _ -> orig
    fixStackAccesses (Special (FunctionStart s _ _))
      = Special (FunctionStart s usedRegs totalStack)
    fixStackAccesses (Load c m r (Reg SP) plus (Imm i))
      = Load c m r (Reg SP) plus (Imm (i + totalStack + (length usedRegs * 4)))
    fixStackAccesses (Store c m r SP plus (Imm i))
      = Store c m r SP plus (Imm (i + totalStack + (length usedRegs * 4)))
    fixStackAccesses (Ret op _ _)
      = Ret op usedRegs totalStack
    fixStackAccesses i
      = i
