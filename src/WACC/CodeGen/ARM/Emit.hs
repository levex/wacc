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

genModifier :: Eq a => [(a, String)] -> a -> String -> String
genModifier t = flip (++) . fromJust . flip lookup t

genCond :: Condition -> String -> String
genCond = genModifier conditions

genSize :: MemAccessType -> String -> String
genSize = genModifier sizes

instance Emit Instruction where
  emit (Special (FunctionStart label usedRegs stackSpace))
    = [".globl ", label, "\n"]
        ++ concatMap emit
          [ Special (LabelDecl label)
          , Push CAl (usedRegs ++ [LR])
          , Push CAl [(R 12)]
          , if stackSpace > 0 then
              Op CAl SubOp (R 13) (R 13) (Imm stackSpace) -- sub sp, sp, stackSpace
            else
              Special Empty
          , Move CAl (R 12) (Reg (R 13)) -- mov fp, sp
          ]

  emit (Special (LabelDecl l))
    = [l, ":\n"]

  emit (Special (SectionStart str))
    = [".section ", str, "\n"]

  emit (Special _)
    = []

  emit (Load c m rt op1 plus op2)
    = [genCond c (genSize m "ldr"), " "] ++
        case op1 of
          Imm i   -> [show rt, ", =", show i, "\n"]
          Label l -> [show rt, ", =", l, "\n"]
          Reg rn  -> case op2 of
            Reg rm -> [show rt, ", [", show rn, ", ",
                        if plus then "" else "-", show rm, "]\n"]
            Imm 0  -> [show rt, ", [", show rn, "]\n"]
            Imm i2 -> [show rt, ", [", show rn, ", ",
                                                  "#", show i2, "]\n"]

  emit (Move c rt op1)
    = [genCond c "mov", " "] ++
        case op1 of
          Imm i  -> [show rt, ", #", show i, "\n"]
          Reg rn -> [show rt, ", ", show rn, "\n"]

  emit (MoveN c rt rn)
    = [genCond c "mvn", " ", show rt, ", ", show rn, "\n"]

  emit (Shift c rt rn st op)
    = [genCond c (map toLower (show st)), " ", show rt, ", ", show rn, ", "] ++
        case op of
          Imm i  -> ["#", show i, "\n"]
          Reg rs -> [show rs, "\n"]

  emit (Push c rs)
    = [genCond c "push", " {",
        intercalate ", " $ map show (sort rs), "}\n"]

  emit (Pop c rs)
    = [genCond c "pop", " {",
        intercalate ", " $ map show (sort rs), "}\n"]

  emit (Branch c op1)
    = case op1 of
        Label lab -> [genCond c "b", " ", lab, "\n"]
        Reg rt    -> [genCond c "bx", " ", show rt, "\n"]

  emit (BranchLink c op1)
    = case op1 of
        Label lab -> [genCond c "bl", " ", lab, "\n"]
        Reg rt    -> [genCond c "blx", " ", show rt, "\n"]

  emit (Compare c rt op1) = do
    [genCond c "cmp", " "] ++
      case op1 of
        Reg rn -> [show rt, ", ", show rn, "\n"]
        Imm i  -> [show rt, ", #", show i, "\n"]

  emit (Store c m rt rn plus op2) = do
    [genCond c (genSize m "str"), " "] ++
      case op2 of
        Reg rm -> [show rt, ", [", show rn, ", ",
                    if plus then "" else "- ", show rm, "]\n"]
        Imm 0  -> [show rt, ", [", show rn, "]\n"]
        Imm i  -> [show rt, ", [", show rn, ", #", show i, "]\n"]

  emit (Op c (ModOp True) rt rn op1) = concatMap emit
      [ Push c [r0, r1, r2, r3, R 12]
      , Move c r0 (Reg rn) -- FIXME: see DivOp and unify these
      , Move c r1 op1
      , BranchLink c (Label "__aeabi_idivmod")
      , Move c rt (Reg r1)
      , Pop c [r0, r1, r2, r3, R 12]]

  emit (Op c (ModOp False) rt rn op1) = concatMap emit
      [ Push c [r0, r1, r2, r3, R 12]
      , Move c r0 (Reg rn) -- FIXME: see DivOp and unify these
      , Move c r1 op1
      , BranchLink c (Label "__aeabi_uidivmod")
      , Move c rt (Reg r1)
      , Pop c [r0, r1, r2, r3, R 12]]

  emit (Op c (DivOp True) rt rn op1) = concatMap emit
      [ Push c [r0, r1, r2, r3, R 12]
      , Move c r0 (Reg rn) -- FIXME: proper regsave and div-by-zero check
      , Move c r1 op1
      , BranchLink c (Label "__aeabi_idiv")
      , Move c rt (Reg r0)
      , Pop c [r0, r1, r2, r3, R 12]]

  emit (Op c (DivOp False) rt rn op1) = concatMap emit
      [ Push c [r0, r1, r2, r3, R 12]
      , Move c r0 (Reg rn) -- FIXME: proper regsave and div-by-zero check
      , Move c r1 op1
      , BranchLink c (Label "__aeabi_uidiv")
      , Move c rt (Reg r0)
      , Pop c [r0, r1, r2, r3, R 12]]

  emit (Op c op rt rn op1)
    = [genCond c (fromJust $ lookup op opTable), " "] ++
      case op1 of
        Reg rm -> [intercalate ", " $ map show [rt, rn, rm],"\n"]
        Imm i  -> [show rt, ", ", show rn, ", #", show i,"\n"]

  emit (SWI i)
    = ["swi #", show i, "\n"]

  emit (Ret (Just op) usedRegs stackSpace) = concatMap emit
      [ Move CAl r0 op
      , if stackSpace > 0 then
          Op CAl AddOp (R 13) (R 13) (Imm stackSpace) -- add sp, sp, stackSpace
        else
          Special Empty
      , Pop CAl [(R 12)]
      , Pop CAl (usedRegs ++ [PC])
      ]

  emit (Ret Nothing usedRegs stackSpace) = concatMap emit
      [ if stackSpace > 0 then
          Op CAl AddOp (R 13) (R 13) (Imm stackSpace) -- add sp, sp, stackSpace
        else
          Special Empty
      , Pop CAl [(R 12)]
      , Pop CAl (usedRegs ++ [PC])
      ]

  emit (PureAsm ss)
    = ss
