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
  { lastRegister :: Register,
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

type Register = Int

registers :: Int -> [Register]
registers
  = flip take $ iterate (+ 1) 0

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
  | FunctionCall  Identifier [Register]
  | VariableDecl  Identifier Type Register
  | StringLit     Identifier String
  | LabelDecl     Identifier
  | Alloc         Register Int
  | SWI           Int
  | Ret           Operand
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
  | Special SpecialLink
  | PureAsm [String]
  deriving (Eq, Show)

skip :: Monad m => m ()
skip = return ()
