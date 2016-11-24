module WACC.CodeGen.ARM.Emit where

import Data.Char
import Data.List
import Data.Maybe
import Control.Monad.Writer

import WACC.CodeGen.Types
import WACC.Parser.Types hiding (Add, Sub, Mul, Div)
import WACC.Semantics.Types

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

sizes :: [(MemAccessType, String)]
sizes = [ (Byte,      "b")
        , (HalfWord,  "h")
        , (Word,      "") ]

opTable :: [(Operation, String)]
opTable = [ (AddOp,  "add")
          , (SubOp,  "sub")
          , (RSubOp, "rsb")
          , (MulOp,  "mul")
          , (OrOp,   "orr")
          , (XorOp,  "eor")
          , (AndOp,  "and")]

nameForReg :: Register -> String
nameForReg = show

genModifier :: Eq a => [(a, String)] -> a -> String -> String
genModifier t = flip (++) . fromJust . flip lookup t

genCond :: Condition -> String -> String
genCond = genModifier conditions

genSize :: MemAccessType -> String -> String
genSize = genModifier sizes

instance Emit Instruction where
  emit (Special (FunctionStart label usedRegs))
    = [".globl ", label, "\n"]
        ++ concatMap emit [Special (LabelDecl label),
                     Push CAl (usedRegs ++ [LR])]

  emit (Special (LabelDecl l))
    = [l, ":\n"]

  emit (Special (SectionStart str))
    = [".section ", str, "\n"]

  emit (Special _)
    = []

  emit (Load c m rt op1 plus op2)
    = [genCond c (genSize m "ldr"), " "] ++
        case op1 of
          Imm i   -> [nameForReg rt, ", =", show i, "\n"]
          Label l -> [nameForReg rt, ", =", l, "\n"]
          Reg rn  -> case op2 of
            Reg rm -> [nameForReg rt, ", [", nameForReg rn, ", ",
                        if plus then "" else "-", nameForReg rm, "]\n"]
            Imm 0  -> [nameForReg rt, ", [", nameForReg rn, "]\n"]
            Imm i2 -> [nameForReg rt, ", [", nameForReg rn, ", ",
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

  emit (Store c m rt rn plus op2) = do
    [genCond c (genSize m "str"), " "] ++
      case op2 of
        Reg rm -> [nameForReg rt, ", [", nameForReg rn, ", ",
                    if plus then "" else "- ", nameForReg rm, "]\n"]
        Imm 0  -> [nameForReg rt, ", [", nameForReg rn, "]\n"]
        Imm i  -> [nameForReg rt, ", [", nameForReg rn, ", #", show i, "]\n"]

  emit (Op c ModOp rt rn op1) = concatMap emit
      [ Push c [r0, r1]
      , Move c r0 (Reg rn) -- FIXME: see DivOp and unify these
      , Move c r1 op1
      , BranchLink c (Label "__aeabi_idivmod")
      , Move c rt (Reg r1)
      , Pop c [r0, r1]]

  emit (Op c DivOp rt rn op1) = concatMap emit
      [ Push c [r0, r1]
      , Move c r0 (Reg rn) -- FIXME: proper regsave and div-by-zero check
      , Move c r1 op1
      , BranchLink c (Label "__aeabi_idiv")
      , Move c rt (Reg r0)
      , Pop c [r0, r1]]

  emit (Op c op rt rn op1)
    = [genCond c (fromJust $ lookup op opTable), " "] ++
      case op1 of
        Reg rm -> [intercalate ", " $ map nameForReg [rt, rn, rm],"\n"]
        Imm i  -> [nameForReg rt, ", ", nameForReg rn, ", #", show i,"\n"]

  emit (SWI i)
    = ["swi #", show i, "\n"]

  emit (Ret op usedRegs) = concatMap emit
      [ Move CAl r0 op,
        Pop CAl (usedRegs ++ [PC]) ]

  emit (PureAsm ss)
    = ss
