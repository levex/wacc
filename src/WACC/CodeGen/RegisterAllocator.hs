{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.RegisterAllocator where
{-
import           Data.Graph
import           Data.Array
import           Data.Tree
import           Data.List
import qualified Data.Map as Map
import           Control.Arrow
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
  = ([r], nodes)

addNodeToGraph :: Register -> RegisterAllocator ()
addNodeToGraph r = do
  s@RegAllocState{..} <- get
  let livingVars = map varID liveVars
  let newEdges = edges interferenceGraph ++ createEdges r livingVars
  let newGraph = buildG ((minimum &&& maximum) $ r : livingVars) newEdges
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
  = (Map.! node) <$> gets colorMap

getUsedColors :: [Register] -> RegisterAllocator [Color]
getUsedColors regs
  = nub <$> mapM getNodeColor regs

getMinColor :: [Register] -> RegisterAllocator Color
getMinColor regs
  = getUsedColors regs >>= (pure . minimum . (\uc -> [1..10] \\ uc))

colorRegs :: [Register] -> RegisterAllocator Bool
colorRegs regs
  = (all id) <$> mapM checkAndColorNode regs

checkAndColorNode :: Register -> RegisterAllocator Bool
checkAndColorNode r = do
  color <- getNodeColor r
  case color of
    -1 -> colorNode r
    _  -> return True

markSpillingReg :: Register -> RegisterAllocator Bool
markSpillingReg r = do
  setNodeColor r (-2)
  return False

colorNode :: Register -> RegisterAllocator Bool
colorNode r = do
  graph <- gets interferenceGraph
  minColor <- getMinColor $ graph ! r
  case minColor of
    -1 -> markSpillingReg r
    _  -> setNodeColor r minColor >> colorCurrent r minColor
  where
    colorCurrent :: Register -> Int -> RegisterAllocator Bool
    colorCurrent r c = do
      graph <- gets interferenceGraph
      setNodeColor r c
      colorRegs $ graph ! r

resetGraph :: RegisterAllocator ()
resetGraph
  = modify (\_ -> initRegAllocState)

getUnallocReg :: [(Register, Color)] -> Register
getUnallocReg ((r, -2) : xs) = r
getUnallocReg (_ : xs) = getUnallocReg xs

targetsRegister :: Instruction -> Register -> Bool
targetsRegister (Op _ _ rd _ _) r
  = r == rd
targetsRegister (Load _ rd _ _ _) r
  = r == rd
targetsRegister (Store _ rd _ _ _) r
  = r == rd
targetsRegister (Move _ rd _) r
  = r == rd
targetsRegister (Pop _ regs) r
  = r `elem` regs
targetsRegister instr r
  = False

spill :: [Instruction] -> Register -> Int -> RegisterAllocator [Instruction]
spill (inst : func) regToSkip line
  | not $ targetsRegister inst regToSkip = notFound (inst : func) regToSkip line
  | otherwise                            = spillThis (inst : func) regToSkip line
  where
    notFound :: [Instruction] -> Register -> Int -> RegisterAllocator [Instruction]
    notFound (inst : func) regToSkip line = do
      spilledCode <- spill func regToSkip (line + 1)
      return (inst : spilledCode)
spill [] _ _ = pure []

spillThis :: [Instruction] -> Register -> Int -> RegisterAllocator [Instruction]
spillThis (instr : func) regToSpill line = do
  return []

spillCode :: [Instruction] -> RegisterAllocator [Instruction]
spillCode function = do
  graph <- gets interferenceGraph
  colMap <- gets colorMap
  let regToSpill = getUnallocReg $ Map.assocs colMap
  spill function regToSpill 0

colorGraph :: RegisterAllocator Bool
colorGraph = do
  graph <- gets interferenceGraph
  let startRegs = map (head.flatten) $ components graph
  colored <- mapM checkAndColorNode startRegs
  return $ and colored

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
-}
