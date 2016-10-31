module WACC.CodeGen where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types
import           WACC.CodeGen.Strings
import           WACC.CodeGen.InstructionGen
import           WACC.CodeGen.EmitARM

generateCode :: Program -> String
generateCode p
  = (concat . execWriter . flip evalStateT CodeGenState . runCodeGen) $ do
      emitLiterals p
      instructions <- generateInstructions p
      generateAssembly instructions
