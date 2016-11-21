{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WACC.CodeGen.Types where

import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Graph
import           Data.Array
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types

type Code = [Instruction]

class Emit a where
  emit :: a -> [String]

data CodeGenState = CodeGenState
  { lastRegister :: Int,
    lastLabelId :: Integer,
    regIdsTable :: Map (Identifier, Integer) Register,
    scopeId :: Integer,
    usedBuiltins :: Set Identifier }


newtype CodeGenerator a = CodeGenerator
  { runCodeGen :: State CodeGenState a }
      deriving (Functor, Applicative, Monad,
                MonadState CodeGenState)

codeGenState :: CodeGenState
codeGenState = CodeGenState
  { lastRegister = 4,
    lastLabelId = 0,
    regIdsTable = Map.empty,
    scopeId = 0,
    usedBuiltins = Set.empty }

type InstructionGenerator a
  = WriterT [Instruction] CodeGenerator a

data RegAllocState = RegAllocState
  { interferenceGraph :: Graph,
    colorMap :: Map Register Color,
    instrCount :: Int,
    liveVars :: [LiveVar] }

newtype RegisterAllocator a = RegisterAllocator
  { runRegAllocator :: State RegAllocState a }
      deriving (Functor, Applicative, Monad,
                MonadState RegAllocState)

data LiveVar = LiveVar
  { varID :: Int,
    liveRange :: Int}

type Color = Int

data Condition
  = CAl -- always
  | CEq -- equal
  | CNe -- not equal
  | CCs -- carry set
  | CCc -- carry clear
  | CMi -- minus
  | CPl -- plus
  | CVs -- overflow
  | CVc -- no overflow
  | CHi -- unsigned higher
  | CLs -- unsigned lower or same
  | CGe -- signed Gt or Equal
  | CLt -- signed Lt
  | CGt -- signed Gt
  | CLe -- signed Lt or Equal
  deriving (Eq, Show)

data Register
  = R Int
  | SP
  | LR
  | PC
  deriving (Eq, Ord)

instance Show Register where
  show (R r)  = "r" ++ show r
  show SP     = "sp"
  show LR     = "lr"
  show PC     = "pc"

data Operand
  = Imm Int
  | Reg Register
  | Label String
  deriving (Eq, Show)

data Operation
  = AddOp
  | SubOp
  | MulOp
  | DivOp
  | ModOp
  | AndOp
  | OrOp
  | XorOp
  deriving (Eq, Show)

data ShiftType
  = LSL
  | LSR
  | ASR
  | ROR
  deriving (Eq, Show)

data SpecialLink
  = FunctionStart Identifier
  | SectionStart  String
  | VariableDecl  Identifier Type Register
  | StringLit     Identifier String
  | LabelDecl     Identifier
  deriving (Eq, Show)

data Instruction
  = Op Condition Operation Register Register Operand
  | Load Condition Register Operand Bool Operand -- Rt, Rn (+/-) Rm/imm
  | Store Condition Register Register Bool Operand
  | Move Condition Register Operand
  | Shift Condition Register Register ShiftType Int
  | Negate Condition Register Operand
  | Push Condition [Register]
  | Pop Condition [Register]
  | Branch Condition Operand
  | BranchLink Condition Operand
  | Compare Condition Register Operand
  | SWI Int
  | Ret Operand
  | Special SpecialLink
  | PureAsm [String]
  deriving (Eq, Show)

skip :: Monad m => m ()
skip = return ()

r0 = R 0
r1 = R 1
r2 = R 2
r3 = R 3
