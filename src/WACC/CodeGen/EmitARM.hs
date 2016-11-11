module WACC.CodeGen.EmitARM where

import Data.Char
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

opTable :: [(Operation, String)]
opTable = [ (AddOp, "add")
          , (SubOp, "sub")
          , (MulOp, "mul")
          , (OrOp , "orr")
          , (XorOp, "eor")
          , (AndOp, "and")]

nameForReg :: Register -> String
nameForReg r = fromMaybe ("t_" ++ show r) $ lookup r regNames

genCond :: Condition -> String -> String
genCond = flip (++) . fromJust . flip lookup conditions

instance Emit Instruction where
  emit (Special (FunctionStart label))
    = [".globl ", label, "\n"]
        ++ concatMap emit [Special (LabelDecl label),
                     Push CAl [14]]

  emit (Special (SWI i))
    = ["swi #", show i, "\n"]

  emit (Special (LabelDecl l))
    = [l, ":\n"]

  emit (Special (Alloc r b)) = concatMap emit
      [ Move CAl 0 (Imm b),
        BranchLink CAl (Label "malloc"),
        Move CAl r (Reg 0) ]

  emit (Special (Ret op)) = concatMap emit
      [ Move CAl 0 op,
        Pop CAl [15] ]

  emit (Special (FunctionCall id regs)) = concatMap emit
      [ BranchLink CAl (Label id) ]

  emit (Special (SectionStart str))
    = [".section ", str, "\n"]

  emit (Special _)
    = []

  emit (Load c rt op1 plus op2)
    = [genCond c "ldr", " "] ++
        case op1 of
          Imm i   -> [nameForReg rt, ", =", show i, "\n"]
          Label l -> [nameForReg rt, ", =", l, "\n"]
          Reg rn  -> case op2 of
            Reg rm -> [nameForReg rt, ", [", nameForReg rn, ", ",
                        if plus then "" else "-", nameForReg rm, "]\n"]
            Imm 0  -> [nameForReg rt, ", [", nameForReg rn, "]\n"]
            Imm i2 -> [nameForReg rt, ", [", nameForReg rn, " ",
                                                  "#", show i2, "]\n"]

  emit (Move c rt op1)
    = [genCond c "mov", " "] ++
        case op1 of
          Imm i  -> [nameForReg rt, ", #", show i, "\n"]
          Reg rn -> [nameForReg rt, ", ", nameForReg rn, "\n"]

  emit (Shift c rt rn st i)
    = [genCond c (map toLower (show st)), " ",
        nameForReg rt, ", ", nameForReg rn, ", #", show i, "\n"]

  emit (Push c rs)
    = [genCond c "push", " {",
        intercalate ", " $ map nameForReg (sort rs), "}\n"]

  emit (Pop c rs)
    = [genCond c "pop", " {",
        intercalate ", " $ map nameForReg (sort rs), "}\n"]

  emit (Branch c op1)
    = case op1 of
        Label lab -> [genCond c "b", " ", lab, "\n"]
        Reg rt    -> [genCond c "bx", " ", nameForReg rt, "\n"]

  emit (BranchLink c op1)
    = case op1 of
        Label lab -> [genCond c "bl", " ", lab, "\n"]
        Reg rt    -> [genCond c "blx", " ", nameForReg rt, "\n"]

  emit (Compare c rt op1) = do
    [genCond c "cmp", " "] ++
      case op1 of
        Reg rn -> [nameForReg rt, ", ", nameForReg rn, "\n"]
        Imm i  -> [nameForReg rt, ", #", show i, "\n"]

  emit (Store c rt rn plus op2) = do
    [genCond c "str", " "] ++
      case op2 of
        Reg rm -> [nameForReg rt, ", [", nameForReg rn, ", ",
                    if plus then "" else "- ", nameForReg rm, "]\n"]
        Imm 0  -> [nameForReg rt, ", [", nameForReg rn, "]\n"]
        Imm i  -> [nameForReg rt, ", [", nameForReg rn, ", #", show i, "]\n"]

  emit (Op c ModOp rt rn op1) = concatMap emit
      [ Push c [0, 1]
      , Move c 0 (Reg rn) -- FIXME: see DivOp and unify these
      , Move c 1 op1
      , BranchLink c (Label "__aeabi_idivmod")
      , Move c rt (Reg 0)]

  emit (Op c DivOp rt rn op1) = concatMap emit
      [ Push c [0, 1]
      , Move c 0 (Reg rn) -- FIXME: proper regsave and div-by-zero check
      , Move c 1 op1
      , BranchLink c (Label "__aeabi_idiv")
      , Move c rt (Reg 0)]

  emit (Op c op rt rn op1)
    = [genCond c (fromJust $ lookup op opTable), " "] ++
      case op1 of
        Reg rm -> [intercalate ", " $ map nameForReg [rt, rn, rm],"\n"]
        Imm i  -> [nameForReg rt, ", ", nameForReg rn, ", #", show i,"\n"]

  emit (PureAsm ss)
    = ss

emitSection :: String -> CodeGenerator ()
emitSection section
  = tell [".section \"", section, "\"\n"]

generateAssembly :: [Instruction] -> [String]
generateAssembly = concatMap emit
