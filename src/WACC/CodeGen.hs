module WACC.CodeGen where

import WACC.Parser.Types
import WACC.CodeGen.Types
import WACC.CodeGen.InstructionGen

generateCode :: Program -> Code
generateCode
  = generateProgram
