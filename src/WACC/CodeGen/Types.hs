{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WACC.CodeGen.Types where

import Control.Monad.State
import Control.Monad.Writer
import WACC.Parser.Types

type Code = [Instruction]

data CodeGenState = CodeGenState
  { lastLiteralId :: Int }

newtype CodeGenerator a = CodeGenerator
  { runCodeGen :: StateT CodeGenState (Writer [String]) a }
      deriving (Functor, Applicative, Monad,
                MonadState CodeGenState,
                MonadWriter [String])

data InstrGenState = InstrGenState { lastRegister :: Register }

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
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  deriving (Eq, Show)

data RegType
  = Temporary
  | Variable
  deriving (Eq, Show)

data SpecialLink
  = FunctionStart String
  | FunctionEnd   String
  | VariableDecl  String Type Register
  deriving (Eq, Show)

data Instruction
  = Op Operation Register Register Operand
  | Load Register Operand Bool Operand -- Rt, Rn (+/-) Rm/imm
  | Store Register Register Bool Operand
  | Move Register Operand
  | Negate Register Operand
  | Push [Register]
  | Pop [Register]
  | Branch Operand
  | BranchLink Operand
  | Compare Register Operand
  | Special SpecialLink
  | PureAsm [String]
  deriving (Eq, Show)

type CondInstr = (Condition, Instruction)

skip :: CodeGenerator ()
skip = return ()
