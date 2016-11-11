{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
module WACC.CodeGen.LinearScanRegisterAlloc where

import           Data.Graph
import           Data.Array
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

-- READ BEFORE COMPLAINING:
--  http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf
--
-- If you complain, fix it and come up with a better solution.
-- Yeah the monad part is fugly. I'll have a look at it later.
-- replaceRegister and the liveRange calculation is nice as is, don't touch it
-- please. -L

totalNumberRegister = 8

data LSRAState = LSRAState
  { lranges :: Map.Map Int (Int, Int) -- reg (start, end)
  , instructions :: [Instruction]
  , active :: [(Int, Int, Int)] -- (reg, start, end)
  , freePool :: [Register]
  , finalAllocations :: [(Register, Register)]
  } deriving (Eq, Show)

initialLSRAState :: [Instruction] -> LSRAState
initialLSRAState p = LSRAState
  { lranges = Map.empty
  , instructions = p
  , active = []
  , freePool = [0..totalNumberRegister]
  , finalAllocations = []
  }

newtype LSRA a = LSRA { unLSRA :: State LSRAState a }
                  deriving (Functor, Applicative, Monad,
                            MonadState LSRAState)

data RegAction = RRead | RWrite | RIgnore

analyzeAccess :: Register -> Instruction -> RegAction
analyzeAccess r (Op _ _ rd r1 (Reg r2))
  | r == rd = RWrite
  | r == r1 || r == r2 = RRead
  | otherwise = RIgnore

analyzeAccess r (Op _ _ rd r1 _)
  | r == rd            = RWrite
  | r == r1            = RRead
  | otherwise          = RIgnore

analyzeAccess r (Load _ rd (Reg r1) _ (Reg r2))
  | r == rd            = RWrite
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Load _ rd _ _ _)
  | r == rd            = RWrite
  | otherwise          = RIgnore

analyzeAccess r (Store _ rd r1 _ (Reg r2))
  | r == rd            = RWrite
  | r == r1 || r == r2 = RRead
  | otherwise          = RIgnore

analyzeAccess r (Store _ rd r1 _ _ )
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

analyzeAccess r (Shift _ r1 r2 _ _)
  | r == r1 || r == r2 = RRead      --- FIXME: is this correct?
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

analyzeAccess _ _ = RIgnore

------------------------------------------------

replace' :: [(Register, Register)] -> Register -> Register
replace' rs r = fromMaybe r (lookup r rs)

replaceRegister :: [(Register, Register)] -> Instruction  -> Instruction
replaceRegister rs (Op a b rd r1 (Reg r2))
  = Op a b (replace' rs rd) (replace' rs r1) (Reg (replace' rs r2))

replaceRegister rs (Op a b rd r1 c)
  = Op a b (replace' rs rd) (replace' rs r1) c

replaceRegister rs (Load a rd (Reg r1) b (Reg r2))
  = Load a (replace' rs rd) (Reg (replace' rs r1)) b (Reg (replace' rs r2))

replaceRegister rs (Load a rd b c d)
  = Load a (replace' rs rd) b c d

replaceRegister rs (Store a rd r1 b (Reg r2))
  = Store a (replace' rs rd) (replace' rs r1) b (Reg (replace' rs r2))

replaceRegister rs (Store a rd r1 b c)
  = Store a (replace' rs rd) (replace' rs r1) b c

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

replaceRegister _ i = i

calcLiveRange :: Register -> Int -> Instruction -> LSRA ()
calcLiveRange r line (analyzeAccess r -> RWrite) = do
  st@LSRAState{..} <- get
  let lr' = case Map.lookup r lranges of
              Nothing     -> Map.insert r (line, line) lranges
              Just (s, _) -> Map.insert r (s, line) lranges
  put st{lranges = lr'}
calcLiveRange r line (analyzeAccess r -> RRead) = do
  st@LSRAState{..} <- get
  let lr' = case Map.lookup r lranges of
              Nothing     ->
                trace ("register " ++ show r ++ " not loaded with value at l"
                    ++ show line) lranges
              Just (s, _) -> Map.insert r (s, line) lranges
  put st{lranges = lr'}
calcLiveRange _ _ _ = return ()

allocateLSRA :: LSRA ()
allocateLSRA = do
  -- calculate live ranges
  ins <- gets instructions
  forM_ [4..10] $ \r ->
    forM_ (zip [0..] ins) $ \(l, i) ->
      calcLiveRange r l i
  -- start LSRA
  lr <- sortBy ((\(s1, _) (s2, _) -> compare s1 s2) . snd)
          <$> Map.toList <$> gets lranges
  forM_ lr $ \i@(r, (s, e)) -> do
    expireOldIntervals i
    len <- length <$> gets active
    if len == totalNumberRegister then
      spillAtInterval i
    else do
      st@LSRAState{..} <- get
      let rt = head freePool
      put st{ freePool = freePool \\ [rt]
            , finalAllocations = (r, rt) : finalAllocations  -- r got assigned to rt
            , active = (r, s, e) : active  -- i is now active
            }
      
  traceM $ show lr

spillAtInterval :: (Int, (Int, Int)) -> LSRA ()
spillAtInterval = undefined

expireOldIntervals :: (Int, (Int, Int)) -> LSRA ()
expireOldIntervals i@(r, (s, e)) = do
  st@LSRAState{..} <- get
  let acts = sortBy (\(_, _, e1) (_, _, e2) -> compare e1 e2) active
  void $ runEitherT (forM_ acts $ \j@(rj, sj, ej) -> do
    if ej > s then
      left () -- exit from the loop
    else
      put st{active = acts \\ [j],     -- remove interval j from active intervals
            freePool = rj : freePool}) -- remove reg j from the freepool

testInstructions :: [Instruction]
testInstructions =
  [ Load CAl 4 (Imm 0) True (Imm 0) -- l0
  , Load CAl 5 (Imm 1) True (Imm 0) -- l1
  , Load CAl 6 (Imm 2) True (Imm 0) -- l2
  , Op   CAl AddOp 7 4 (Reg 5) -- l3
  , Op   CAl AddOp 8 7 (Reg 6) -- l4
  ]

runLSRA = allocateFuncRegisters

allocateFuncRegisters :: [Instruction] -> [Instruction]
allocateFuncRegisters p
  = map (replaceRegister allocs) p
  where
    final = (execState . unLSRA) allocateLSRA (initialLSRAState p)
    instrs = instructions final
    allocs = finalAllocations final
