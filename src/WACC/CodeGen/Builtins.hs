{-# LANGUAGE ViewPatterns #-}
module WACC.CodeGen.Builtins where

import           Control.Arrow
import           Control.Monad.Writer
import           Data.List
import           WACC.Parser.Types
import           WACC.CodeGen.Types
import           WACC.CodeGen.EmitARM

isBuiltinSpecial :: Instruction -> (Bool, String)
isBuiltinSpecial (Special (FunctionCall str _))
  = ((isPrefixOf "__builtin_") &&& id) str
isBuiltinSpecial _
  = (False, undefined)

emitBuiltinFunction :: Instruction -> [Instruction]
emitBuiltinFunction (isBuiltinSpecial -> (True, s))
  = generateBuiltinCall s []
emitBuiltinFunction _
  = []

generateBuiltinInstructions :: Code -> [Instruction]
generateBuiltinInstructions = concatMap emitBuiltinFunction

-- __builtin_fmt_read_char = " %c\0"
generateBuiltinCall :: Identifier -> [Expr] -> [Instruction]
generateBuiltinCall "__builtin_Read_TChar" _ = 
       [ Special (FunctionStart "__builtin_Read_TChar")
       , Move       CAl 1 (Reg 0)
       , Load       CAl 0 (Label "__builtin_fmt_read_char") True (Imm 0)
       , Op         CAl AddOp 0 0 (Imm 4)
       , BranchLink CAl (Label "scanf")
       , Pop        CAl [15]
       ]

-- __builtin_fmt_read_int = " %d\0"
generateBuiltinCall "__builtin_Read_TInt" _ =
       [ Special (FunctionStart "__builtin_Read_TInt")
       , Move       CAl 1 (Reg 0)
       , Load       CAl 0 (Label "__builtin_fmt_read_int") True (Imm 0)
       , Op         CAl AddOp 0 0 (Imm 4)
       , BranchLink CAl (Label "scanf")
       , Pop        CAl [15]
       ]

generateBuiltinCall "__builtin_Print_TChar" _ =
       [ Special (FunctionStart "__builtin_Print_TChar")
       , Branch CAl (Label "putchar")]

-- __builtin_fmt_int = "%d\n"
generateBuiltinCall "__builtin_Print_TInt" _ =
       [ Special (FunctionStart "__builtin_Print_TInt")
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
generateBuiltinCall "__builtin_Print_TBool" _ =
       [ Special (FunctionStart "__builtin_Print_TBool")
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
generateBuiltinCall "__builtin_Print_TString" _ =
       [ Special (FunctionStart "__builtin_Print_TString")
       , Load       CAl 1 (Reg 0) True (Imm 0)
       , Op         CAl AddOp 2 0 (Imm 4)
       , Load       CAl 0 (Label "__builtin_fmt_string") True (Imm 0)
       , Op         CAl AddOp 0 0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl 0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [15]
       ]

generateBuiltinCall "__builtin_Print_TRef" _
  = []

generateBuiltinCall s _
  = [BranchLink CAl (Label $ "ERR_" ++ s)]

