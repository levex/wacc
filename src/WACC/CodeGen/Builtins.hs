{-# LANGUAGE ViewPatterns #-}
module WACC.CodeGen.Builtins where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Arrow
import           Control.Monad.Writer
import           Data.List
import           WACC.Parser.Types
import           WACC.CodeGen.Types

generateBuiltins :: Set Identifier -> CodeGenerator [Instruction]
generateBuiltins
  = execWriterT . mapM_ generateBuiltinCall . Set.toList

-- __builtin_fmt_read_char = " %c\0"
generateBuiltinCall :: Identifier -> InstructionGenerator ()
generateBuiltinCall "__builtin_Read_TChar" =
  tell [ Special (FunctionStart "__builtin_Read_TChar" [] 0)
       , Op         CAl SubOp r1 (R 12) (Imm 4)
       , Load       CAl Word r0 (Label "__builtin_fmt_read_char") True (Imm 0)
       , Op         CAl AddOp r0 r0 (Imm 4)
       , BranchLink CAl (Label "scanf")
       , Load       CAl Word r0 (Reg SP) True (Imm $ -4)
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

-- __builtin_fmt_read_int = " %d\0"
generateBuiltinCall "__builtin_Read_TInt" =
  tell [ Special (FunctionStart "__builtin_Read_TInt" [] 0)
       , Op         CAl SubOp r1 SP (Imm 4)
       , Load       CAl Word r0 (Label "__builtin_fmt_read_int") True (Imm 0)
       , Op         CAl AddOp r0 r0 (Imm 4)
       , BranchLink CAl (Label "scanf")
       , Load       CAl Word r0 (Reg SP) True (Imm $ -4)
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

generateBuiltinCall "__builtin_Print_TChar" =
  tell [ Special (FunctionStart "__builtin_Print_TChar" [] 0)
       , Load       CAl Word r0 (Reg $ R 12) True (Imm 8)
       , BranchLink CAl (Label "putchar")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]]

-- __builtin_fmt_int = "%d\n"
generateBuiltinCall "__builtin_Print_TInt" =
  tell [ Special (FunctionStart "__builtin_Print_TInt" [] 0)
       , Load       CAl Word r1 (Reg $ R 12) True (Imm 8)
       , Load       CAl Word r0 (Label "__builtin_fmt_int") True (Imm 0)
       , Op         CAl AddOp r0 r0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl r0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

-- __builtin_str_true = "true"
-- __builtin_str_true = "false"
generateBuiltinCall "__builtin_Print_TBool" =
  tell [ Special (FunctionStart "__builtin_Print_TBool" [] 0)
       , Load       CAl Word r0 (Reg $ R 12) True (Imm 8)
       , Compare    CAl r0 (Imm 0)
       , Load       CNe Word r0 (Label "__builtin_str_true") True (Imm 0)
       , Load       CEq Word r0 (Label "__builtin_str_false") True (Imm 0)
       , Op         CAl AddOp r0 r0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl r0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

-- __builtin_fmt_string = "%.*s\0"
generateBuiltinCall "__builtin_Print_TString" =
  tell [ Special (FunctionStart "__builtin_Print_TString" [] 0)
       , Load       CAl Word r0 (Reg $ R 12) True (Imm 8)
       , Load       CAl Word r1 (Reg r0) True (Imm 0)
       , Op         CAl AddOp r2 r0 (Imm 4)
       , Load       CAl Word r0 (Label "__builtin_fmt_string") True (Imm 0)
       , Op         CAl AddOp r0 r0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl r0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

generateBuiltinCall "__builtin_Print_TCharArray" =
  tell [ Special (FunctionStart "__builtin_Print_TCharArray" [])
       , Push       CAl [r0, R 4, R 5, R 6]
       , Load       CAl Word (R 4) (Reg SP) True (Imm 20)
       , Load       CAl Word (R 6) (Reg $ R 4) True (Imm 0)
       , Shift      CAl (R 6) (R 6) LSL 2
       , Op         CAl AddOp (R 6) (R 6) (Imm 4)
       , Move       CAl (R 5) (Imm 4)
       , Special        (LabelDecl "__builtin_tchararray_l1")
       , Compare    CAl (R 5) (Reg $ R 6)
       , Branch     CEq (Label "__builtin_tchararray_l2")
       , Load       CAl Word r0 (Reg $ R 4) True (Reg $ R 5)
       , BranchLink CAl (Label "putchar")
       , Op         CAl AddOp (R 5) (R 5) (Imm 4)
       , Branch     CAl (Label "__builtin_tchararray_l1")
       , Special        (LabelDecl "__builtin_tchararray_l2")
       , Pop        CAl [r0, R 4, R 5, R 6]
       , Pop        CAl [PC]]

generateBuiltinCall "__builtin_Print_TRef" =
  tell [ Special (FunctionStart "__builtin_Print_TRef" [] 0)
       , Load       CAl Word r1 (Reg $ R 12) True (Imm 8)
       , Load       CAl Word r0 (Label "__builtin_fmt_ref") True (Imm 0)
       , Op         CAl AddOp r0 r0 (Imm 4)
       , BranchLink CAl (Label "printf")
       , Move       CAl r0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

generateBuiltinCall "__builtin_PrintLn" =
  tell [ Special (FunctionStart "__builtin_PrintLn" [] 0)
       , Load       CAl Word r0 (Label "__builtin_str_newline") True (Imm 0)
       , Op         CAl AddOp r0 r0 (Imm 4)
       , BranchLink CAl (Label "puts")
       , Move       CAl r0 (Imm 0)
       , BranchLink CAl (Label "fflush")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

generateBuiltinCall "__builtin_Exit" =
  tell [ Special (FunctionStart "__builtin_Exit" [] 0)
       , Load       CAl Word r0 (Reg SP) True (Imm 4)
       , BranchLink CAl (Label "exit")
       ]

generateBuiltinCall "__builtin_Alloc" =
  tell [ Special (FunctionStart "__builtin_Alloc" [] 0)
       , Load       CAl Word r0 (Reg SP) True (Imm 4)
       , BranchLink CAl (Label "malloc")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

generateBuiltinCall "__builtin_Free" =
  tell [ Special (FunctionStart "__builtin_Free" [] 0)
       , Load       CAl Word r0 (Reg SP) True (Imm 4)
       , Compare    CAl r0 (Imm 0)
       , Load       CEq Word r0 (Label "__builtin_nullptr_str") True (Imm 0)
       , Branch     CEq (Label "__builtin_ThrowError")
       , BranchLink CAl (Label "free")
       , Pop        CAl [R 12]
       , Pop        CAl [PC]
       ]

generateBuiltinCall "__builtin_ThrowNullptr" =
  tell [ Special (FunctionStart "__builtin_ThrowNullptr" [] 0)
       , Load       CAl Word r0 (Label "__builtin_nullptr_str") True (Imm 0)
       , BranchLink CAl (Label "__builtin_ThrowError")
       ]

generateBuiltinCall "__builtin_ThrowArrayBounds" =
  tell [ Special (FunctionStart "__builtin_ThrowArrayBounds" [] 0)
       , Load       CAl Word r0 (Label "__builtin_arraybounds_str") True (Imm 0)
       , BranchLink CAl (Label "__builtin_ThrowError")
       ]

generateBuiltinCall "__builtin_ThrowOverflow" =
  tell [ Special (FunctionStart "__builtin_ThrowOverflow" [] 0)
       , Load       CAl Word r0 (Label "__builtin_overflow_str") True (Imm 0)
       , BranchLink CAl (Label "__builtin_ThrowError")
       ]

generateBuiltinCall "__builtin_ThrowDivByZero" =
  tell [ Special (FunctionStart "__builtin_ThrowDivByZero" [] 0)
       , Load       CAl Word r0 (Label "__builtin_divbyzero_str") True (Imm 0)
       , BranchLink CAl (Label "__builtin_ThrowError")
       ]

generateBuiltinCall "__builtin_ThrowError" =
  tell [ Special (FunctionStart "__builtin_ThrowError" [] 0)
       , Push       CAl [r0]
       , BranchLink CAl (Label "__builtin_Print_TString")
       , Op         CAl AddOp SP SP (Imm 4)
       , Move       CAl r0 (Imm $ -1)
       , Branch     CAl (Label "exit")
       ]
