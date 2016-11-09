module WACC.CodeGen.Builtins where

import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

generateBuiltinCall :: Identifier -> [Expr] -> InstructionGenerator ()
generateBuiltinCall "__builtin_Read_TChar" [e]
  = return ()

generateBuiltinCall "__builtin_Read_TInt" [e]
  = return ()

generateBuiltinCall "__builtin_Print_TChar" [e] =
  tell [Branch CAl (Label "putchar")]

-- __builtin_fmt_int = "%d\n"
generateBuiltinCall "__builtin_Print_TInt" [e] =
  tell [ Push       CAl [14]
       , Move       CAl 1 (Reg 0)
       , Load       CAl 0 (Label "__builtin_fmt_int") True (Imm 0)
       , Op         CAl AddOp 0 0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl 0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [15]
       ]

-- __builtin_str_true = "true"
-- __builtin_str_true = "false"
generateBuiltinCall "__builtin_Print_TBool" [e] =
  tell [ Push       CAl [14]
       , Compare    CAl 0 (Imm 0)
       , Load       CNe 0 (Label "__builtin_str_true") True (Imm 0)
       , Load       CEq 0 (Label "__builtin_str_false") True (Imm 0)
       , Op         CAl AddOp 0 0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl 0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [15]
       ]

-- __builtin_fmt_string = "%.*s\0"
generateBuiltinCall "__builtin_Print_TString" [e] =
  tell [ Push       CAl [14]
       , Load       CAl 1 (Reg 0) True (Imm 0)
       , Op         CAl AddOp 2 0 (Imm 4)
       , Load       CAl 0 (Label "__builtin_fmt_string") True (Imm 0)
       , Op         CAl AddOp 0 0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl 0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [15]
       ]

generateBuiltinCall "__builtin_Print_TRef" [e]
  = return ()

