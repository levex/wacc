module WACC.CodeGen where

import           Debug.Trace
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
      traceM "Instructions:\n\n"
      traceM (unlines (map show instructions))
      traceM "\n-----------------------------\n\n"
      emitSection ".data"
      emitLiterals instructions
      emitBuiltinStrings instructions
      emitSection ".text"
      generateAssembly instructions
