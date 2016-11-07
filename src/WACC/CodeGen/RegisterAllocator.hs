{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.RegisterAllocator where

import           Data.Graph
import           Data.Array
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

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
