module WACC.CodeGen.EmitARM where

import Data.List
import Data.Maybe
import Control.Monad.Writer

import WACC.CodeGen.Types
import WACC.Parser.Types hiding (Add, Sub, Mul, Div)
import WACC.Semantics.Types

regNames :: [(Register, String)]
regNames = [ (0, "r0")
           , (1, "r1")
           , (2, "r2")
           , (3, "r3")
           , (4, "r4")
           , (5, "r5")
           , (6, "r6")
           , (7, "r7")
           , (8, "r8")
           , (9, "r9")
           , (10, "r10")
           , (11, "r11")
           , (12, "r12")
           , (13, "sp")
           , (14, "lr")
           , (15, "pc")]

conditions :: [(Condition, String)]
conditions = [ (CAl, "")
            ,  (CEq, "eq")
            ,  (CNe, "ne")
            ,  (CCs, "cs")
            ,  (CCc, "cc")
            ,  (CMi, "mi")
            ,  (CPl, "pl")
            ,  (CVs, "vs")
            ,  (CVc, "vc")
            ,  (CHi, "hi")
            ,  (CLs, "ls")
            ,  (CGe, "ge")
            ,  (CLt, "lt")
            ,  (CGt, "gt")
            ,  (CLe, "le") ]

-- FIXME: this needs moving
armFunctionEnter :: [Instruction]
armFunctionEnter = [
                    Push $ 14 : [4..11],   -- save registers
                    Move 4 0,  -- push arguments to scratch
                    Move 5 1,
                    Move 6 2,
                    Move 7 3
                    ] -- lr and r4-r11

-- FIXME: this needs moving
armFunctionExit :: [Instruction]
armFunctionExit = [Pop $ 15 : [4..11]] -- pc and r4-r11

nameForReg :: Register -> String
nameForReg = fromJust . flip lookup regNames

genCond :: Condition -> String -> String
genCond = flip (++) . fromJust . flip lookup conditions

emitBranchInstr :: CondInstr -> CodeGenerator ()
emitBranchInstr (cond, Branch reg) = tell ["bl ", nameForReg reg, "\n"]

emitStackInstr :: CondInstr -> CodeGenerator ()
emitStackInstr (cond, Push [])
  = return ()
emitStackInstr (cond, Pop [])
  = return ()
emitStackInstr (cond, Push regs)
  = tell ["stmdb sp!, {", intercalate ", " (map nameForReg regs) ++ "}"
          , "\n"]
emitStackInstr (cond, Pop regs)
  = tell ["ldmia sp!, {", intercalate ", " (map nameForReg regs) ++ "}"
          , "\n"]

emitArithmeticInstr :: CondInstr -> CodeGenerator ()
emitArithmeticInstr (cond, Add d a b)
  = tell [genCond cond "add ", intercalate ", " (map nameForReg [d, a, b])
          , "\n"]
emitArithmeticInstr (cond, Sub d a b)
  = tell [genCond cond "sub ", intercalate ", " (map nameForReg [d, a, b])
          , "\n"]
emitArithmeticInstr (cond, Mul d a b)
  = tell [genCond cond "mul ", intercalate ", " (map nameForReg [d, a, b])
          , "\n"]
emitArithmeticInstr (cond, Div d a b)
  = tell [genCond cond "udiv ", intercalate ", " (map nameForReg [d, a, b])
          , "\n"]

emitLMIInstr :: CondInstr -> CodeGenerator ()
emitLMIInstr (cond, LoadMemoryImmediate rt rn off)
  = tell [genCond cond "ldr", " ", nameForReg rt, ", [", nameForReg rn,
          ", #", show off, "]\n"]

emitLMRInstr :: CondInstr -> CodeGenerator ()
emitLMRInstr (cond, LoadMemoryRegister rt rn _ (-1))
  = tell [genCond cond "ldr", " ", nameForReg rt, ", [", nameForReg rn, "]\n"]
emitLMRInstr (cond, LoadMemoryRegister rt rn plus rm)
  = tell [genCond cond "ldr", " ", nameForReg rt, ", [", nameForReg rn,
          if plus then " + " else " - ", nameForReg rm, "]\n"]
