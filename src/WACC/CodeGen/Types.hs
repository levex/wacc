{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module WACC.CodeGen.Types where

import Control.Monad.State
import Control.Monad.Writer

type Code = [Instruction]

data CodeGenState = CodeGenState

newtype CodeGenerator a = CodeGenerator
  { runCodeGen :: StateT CodeGenState (Writer [Instruction]) a }
      deriving (Functor, Applicative, Monad,
                MonadState CodeGenState,
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

data Instruction
  = Add Condition Register Register 
  | Sub Condition Register Register 
  | Mul Condition Register Register 
  | Div Condition Register Register 
  | Branch Register
  deriving (Eq, Show)
