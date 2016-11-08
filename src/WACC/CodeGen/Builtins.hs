module WACC.CodeGen.Builtins where

import           WACC.Parser.Types
import           WACC.CodeGen.Types

generateBuiltinCall :: Identifier -> [Expr] -> InstructionGenerator ()
generateBuiltinCall "__builtin_Read_TChar" [e]
  = return ()

generateBuiltinCall "__builtin_Read_TInt" [e]
  = return ()

generateBuiltinCall "__builtin_Print_TChar" [e]
  = return ()

generateBuiltinCall "__builtin_Print_TInt" [e]
  = return ()

generateBuiltinCall "__builtin_Print_TBool" [e]
  = return ()

generateBuiltinCall "__builtin_Print_TString" [e]
  = return ()

generateBuiltinCall "__builtin_Print_TRef" [e]
  = return ()

