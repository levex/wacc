module WACC.CodeGen.EmitARM where

import Data.List
import Data.Maybe
import Control.Monad.Writer

import WACC.CodeGen.Types
import WACC.Parser.Types
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

nameForReg :: Register -> String
nameForReg = fromJust . flip lookup regNames

genCond :: Condition -> String -> String
genCond = flip (++) . fromJust . flip lookup conditions

emitBranchInstr :: CondInstr -> CodeGenerator ()
emitBranchInstr (cond, Branch reg) = tell ["bl " ++ nameForReg reg ]

emitStackInstr :: CondInstr -> CodeGenerator ()
emitStackInstr (cond, Push [])
  = return ()
emitStackInstr (cond, Pop [])
  = return ()
emitStackInstr (cond, Push regs)
  = tell ["stmdb sp!, {" ++ intercalate ", " (map nameForReg regs) ++ "}"]
emitStackInstr (cond, Pop regs)
  = tell ["ldmia sp!, {" ++ intercalate ", " (map nameForReg regs) ++ "}"]
