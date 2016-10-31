module WACC.CodeGen.InstructionGen where

import           Control.Monad
import           WACC.Parser.Types
import           WACC.CodeGen.Types

generateFunction :: Definition -> CodeGenerator Code
generateFunction (FunDef (ident, _) stmt)
  = undefined

generateInstructions :: Program -> CodeGenerator Code
generateInstructions
  = undefined
