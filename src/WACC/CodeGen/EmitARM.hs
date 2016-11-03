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
emitInstruction (Special (LabelDecl l))
  = tell [l, ":\n"]
emitInstruction (Special (Alloc r b))
  = mapM_ emitInstruction
    [ Move CAl 0 (Imm b),
      BranchLink CAl (Label "malloc"),
      Move CAl r (Reg 0) ]
emitInstruction (Special _)
  = skip
emitInstruction (Load c rt op1 plus op2) = do
  tell $ [genCond c "ldr", " "]
  case op1 of
    Imm i   -> tell [nameForReg rt, ", =", show i, "\n"]
    Label l -> tell [nameForReg rt, ", =", l, "\n"]
    Reg rn  -> case op2 of
      Reg rm -> tell [nameForReg rt, ", [", nameForReg rn, " ",
                  if plus then "" else "-", nameForReg rm, "]\n"]
      Imm 0  -> tell [nameForReg rt, ", [", nameForReg rn, "]\n"]
      Imm i2 -> tell [nameForReg rt, ", [", nameForReg rn, " ",
                                            "#", show i2, "]\n"]
emitInstruction (Move c rt op1) = do
  tell [genCond c "mov", " "]
  case op1 of
    Imm i -> tell [nameForReg rt, ", #", show i, "\n"]
    Reg rn -> tell [nameForReg rt, ", ", nameForReg rn, "\n"]
emitInstruction (Push rs)
  = tell ["push {", intercalate ", " $ map nameForReg (sort rs), "}\n"]
emitInstruction (Pop rs)
  = tell ["pop {", intercalate ", " $ map nameForReg (sort rs), "}\n"]
emitInstruction (Branch c op1)
  = case op1 of
      Label lab -> tell [genCond c "b", " ", lab, "\n"]
      Reg rt    -> tell [genCond c "bx", " ", nameForReg rt, "\n"]
emitInstruction (BranchLink c op1)
  = case op1 of
      Label lab -> tell [genCond c "bl", " ", lab, "\n"]
      Reg rt    -> tell [genCond c "blx", " ", nameForReg rt, "\n"]
emitInstruction (Compare c rt op1) = do
  tell [genCond c "cmp", " "]
  case op1 of
    Reg rn -> tell [nameForReg rt, ", ", nameForReg rn, "\n"]
    Imm i  -> tell [nameForReg rt, ", #", show i, "\n"]
emitInstruction (Store c rt rn plus op2) = do
  tell [genCond c "str", " "]
  case op2 of
    Reg rm -> tell [nameForReg rt, ", [", nameForReg rn, " ",
                if plus then "" else "-", nameForReg rm, "]\n"]
    Imm 0  -> tell [nameForReg rt, ", [", nameForReg rn, "]\n"]
    Imm i  -> tell [nameForReg rt, ", [", nameForReg rn, ", #", show i, "]\n"]
emitInstruction (PureAsm ss)
  = tell ss

generateAssembly :: [Instruction] -> CodeGenerator ()
generateAssembly is = do
  tell [".section \".text\"\n"]
  mapM_ emitInstruction is
