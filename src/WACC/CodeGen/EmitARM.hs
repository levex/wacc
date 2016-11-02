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

nameForReg :: Register -> String
nameForReg r = fromMaybe ("t_" ++ show r) $ lookup r regNames

genCond :: Condition -> String -> String
genCond = flip (++) . fromJust . flip lookup conditions

emitInstruction :: Instruction -> CodeGenerator ()
emitInstruction (Special (FunctionStart label))
  = tell [label, ": \npush {lr}\n"]
emitInstruction (Special (FunctionEnd _))
  = tell ["pop {pc}"]
emitInstruction (Special (SWI i))
  = tell ["swi #", show i, "\n"]
emitInstruction (Special _)
  = skip
emitInstruction (Load rt op1 plus op2)
  = case op1 of
      Imm i  -> tell ["ldr ", nameForReg rt, ", =", show i, "\n"]
      Reg rn -> case op2 of
          Reg rm -> tell ["ldr ", nameForReg rt, ", [", nameForReg rn,
                      if plus then " + " else " - ", nameForReg rm, "]\n"]
          Imm 0  -> tell ["ldr ", nameForReg rt, ", [", nameForReg rn, "]\n"]
emitInstruction (Move rt op1)
  = case op1 of
      Imm i -> tell ["mov ", nameForReg rt, ", #", show i, "\n"]
      Reg rn -> tell ["mov ", nameForReg rt, ", ", nameForReg rn, "\n"]
emitInstruction (Push rs)
  = tell ["push {", intercalate ", " $ map nameForReg (sort rs), "}\n"]
emitInstruction (Pop rs)
  = tell ["pop {", intercalate ", " $ map nameForReg (sort rs), "}\n"]
emitInstruction (Branch op1)
  = case op1 of
      Label lab -> tell ["b ", lab, "\n"]
      Reg rt    -> tell ["bx ", nameForReg rt, "\n"]
emitInstruction (BranchLink op1)
  = case op1 of
      Label lab -> tell ["bl ", lab, "\n"]
      Reg rt    -> tell ["blx ", nameForReg rt, "\n"]
emitInstruction (Compare rt op1)
  = case op1 of
      Reg rn -> tell ["cmp ", nameForReg rt, ", ", nameForReg rn, "\n"]
      Imm i  -> tell ["cmp ", nameForReg rt, ", #", show i, "\n"]
emitInstruction (Store rt rn plus op2)
  = case op2 of
      Reg rm -> tell ["str ", nameForReg rt, ", [", nameForReg rn,
                  if plus then " + " else " - ", nameForReg rm, "]\n"]
      Imm 0  -> tell ["str ", nameForReg rt, ", [", nameForReg rn, "]\n"]
      Imm i  -> tell ["str ", nameForReg rt, ", [", nameForReg rn, ", #",
                  show i, "]\n"];
emitInstruction (PureAsm ss)
  = tell ss

generateAssembly :: [Instruction] -> CodeGenerator ()
generateAssembly is = do
  tell [".section \".text\"\n"]
  mapM_ emitInstruction is
