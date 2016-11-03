module WACC.CodeGen where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types
import           WACC.CodeGen.Strings
import           WACC.CodeGen.Functions
import           WACC.CodeGen.EmitARM
import           WACC.CodeGen.InstructionGen

cgState :: CodeGenState
cgState = CodeGenState { lastLiteralId = 0 }

generateCode :: Program -> String
generateCode p
  = (concat . execWriter . flip evalStateT cgState . runCodeGen) $ do
      let instructions = generateInstructions p
      tell [show instructions, "\n"]
      emitLiterals instructions
      generateAssembly instructions
