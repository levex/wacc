module WACC.CodeGen where

import           Debug.Trace
import           Control.Arrow
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Builtins
import           WACC.CodeGen.Types
import           WACC.CodeGen.Strings
import           WACC.CodeGen.Functions
import           WACC.CodeGen.Emit
import           WACC.CodeGen.InstructionGen
import           WACC.CodeGen.LinearScanRegisterAlloc
import           WACC.CodeGen.ARM.CallingConvention
import           WACC.CodeGen.ARM.Emit

generateCode :: Program -> String
generateCode p
  = flip evalState codeGenState . runCodeGen $ do
      functions <- generateInstructions p
      let instructions = concatMap allocateFuncRegisters functions
      builtins <- gets usedBuiltins
      builtinInstructions <- generateBuiltins builtins
      liftM concat . sequence $
        [ emitSection ".data"
        , emitLiterals instructions
        , emitBuiltinStrings builtins
        , return ".ltorg\n"
        , emitSection ".text"
        , emitAssembly builtinInstructions
        , emitAssembly instructions
        ]
