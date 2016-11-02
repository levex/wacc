{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WACC.CodeGen.Types where

import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types

type Code = [Instruction]

data CodeGenState = CodeGenState
  { lastLiteralId :: Int }

newtype CodeGenerator a = CodeGenerator
  { runCodeGen :: StateT CodeGenState (Writer [String]) a }
      deriving (Functor, Applicative, Monad,
                MonadState CodeGenState,
                MonadWriter [String])

data InstrGenState = InstrGenState
  { lastRegister :: Register,
    lastLabelId :: Integer,
    regIdsTable :: Map Identifier Register }

newtype InstructionGenerator a = InstructionGenerator
  { runInstrGen :: StateT InstrGenState (Writer [Instruction]) a }
      deriving (Functor, Applicative, Monad,
                MonadState InstrGenState,
                MonadWriter [Instruction])

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
  deriving (Eq, Show)

data RegType
  = Temporary
  | Variable
  deriving (Eq, Show)

data SpecialLink
  = FunctionStart String
  | FunctionEnd   String
  | VariableDecl  String Type Register
  | StringLit     String String
  | LabelDecl     String
  | SWI           Int
  deriving (Eq, Show)

data Instruction
  = Op Condition Operation Register Register Operand
  | Load Condition Register Operand Bool Operand -- Rt, Rn (+/-) Rm/imm
  | Store Condition Register Register Bool Operand
  | Move Condition Register Operand
  | Negate Condition Register Operand
  | Push [Register]
  | Pop [Register]
  | Branch Condition Operand
  | BranchLink Condition Operand
  | Compare Condition Register Operand
  | Special SpecialLink
  | PureAsm [String]
  deriving (Eq, Show)

type CondInstr = (Condition, Instruction)

skip :: Monad m => m ()
skip = return ()
