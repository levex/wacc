{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.RegisterAllocator where

import           Data.Graph
import           Data.Array
import           Data.Tree
import           Data.List
import qualified Data.Map as Map
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

setNodeColor :: Register -> Color -> RegisterAllocator ()
setNodeColor node c
  = modify (\s@RegAllocState{..} -> s{colorMap = Map.update (\_ -> Just c) node colorMap})

getNodeColor :: Register -> RegisterAllocator Color
getNodeColor node
  = gets colorMap >>= (return . (Map.! node))

getUsedColors :: [Register] -> RegisterAllocator [Color]
getUsedColors regs
  = mapM getNodeColor regs >>= (\x -> return $ nub x)

getMinColor :: [Register] -> RegisterAllocator Color
getMinColor regs
  = getUsedColors regs >>= (pure . minimum . (\uc -> [1..10] \\ uc))

colorRegs :: [Register] -> RegisterAllocator Bool
colorRegs regs
  = mapM checkAndColorNode regs >>= (return . (all id))

checkAndColorNode :: Register -> RegisterAllocator Bool
checkAndColorNode r = do
  color <- getNodeColor r
  case color of
    -1 -> colorNode r
    _  -> return True

colorNode :: Register -> RegisterAllocator Bool
colorNode r = do
  graph <- gets interferenceGraph
  minColor <- getMinColor $ graph ! r
  case minColor of
    -1 -> return False
    _  -> colorRegs $ graph ! r

resetGraph :: RegisterAllocator ()
resetGraph
  = modify (\_ -> initRegAllocState)

spillCode :: [Instruction] -> RegisterAllocator [Instruction]
spillCode function
  = return []

colorGraph :: RegisterAllocator Bool
colorGraph = do
  graph <- gets interferenceGraph
  let startRegs = map (head.flatten) $ components graph
  colored <- mapM checkAndColorNode startRegs
  return $ all id colored

allocate :: [Instruction] -> Bool -> RegisterAllocator [Instruction]
allocate function True
  = return []
allocate function False = do
  newFunc <- spillCode function
  resetGraph
  allocateRegisters newFunc

allocateRegisters :: [Instruction] -> RegisterAllocator [Instruction]
allocateRegisters function = do
  buildGraph function
  success <- colorGraph
  allocate function success
  return function -- temporary

initRegAllocState :: RegAllocState
initRegAllocState
  = RegAllocState (array (0, -1) []) Map.empty 0 []

allocateFuncRegisters :: [Instruction] -> [Instruction]
allocateFuncRegisters function
  = evalState (runRegAllocator $ allocateRegisters function) initRegAllocState
