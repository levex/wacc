module WACC.CodeGen.InstructionGen where

import           WACC.Parser.Types
import           WACC.CodeGen.Types

generateFunction :: Definition -> Code
generateFunction ((ident, _), stmt)
  = undefined

generateProgram :: Program -> Code
generateProgram
  = concatMap generateFunction
