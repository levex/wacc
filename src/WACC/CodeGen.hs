module WACC.CodeGen where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types
import           WACC.CodeGen.Strings
import           WACC.CodeGen.InstructionGen
import           WACC.CodeGen.EmitARM

cgState :: CodeGenState
cgState = CodeGenState { lastLiteralId = 0 }

generateCode :: Program -> String
generateCode p
  = (concat . execWriter . flip evalStateT cgState . runCodeGen) $ do
      emitLiterals p
      -- instructions <- generateInstructions p
      -- generateAssembly instructions
