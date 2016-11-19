module WACC.CodeGen.Emit where

import           WACC.CodeGen.Types

import           WACC.CodeGen.ARM.Emit

emitSection :: String -> CodeGenerator String
emitSection section = pure . concat $ [".section \"", section, "\"\n"]

emitAssembly :: [Instruction] -> CodeGenerator String
emitAssembly = pure . concat . concatMap emit
