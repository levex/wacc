{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.Strings where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Char
import           Data.Bool
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

emitString :: Identifier -> String -> CodeGenerator String
emitString id str
  = return $ concat [".balign 16\n",
                     "  ", id, ":\n",
--                     "    ", ".word ", show . length $ str, "\n",
                     "    ", ".asciz ", str, "\n"]

emitLiteral :: Instruction -> CodeGenerator String
emitLiteral (Special (StringLit id str))
  = emitString id (show str)
emitLiteral (Special (GlobVarDef id (Lit (INT i))))
  = return $ concat ["  ", id, ": .word ", show i, "\n"]
emitLiteral (Special (GlobVarDef id (Lit (BOOL b))))
  = return $ concat ["  ", id, ": .word ", show (bool 0 1 b), "\n"]
emitLiteral (Special (GlobVarDef id (Lit (CHAR c))))
  = return $ concat ["  ", id, ": .word ", show (ord c), "\n"]
emitLiteral (Special (GlobVarDef id (Ident "NULL")))
  = return $ concat ["  ", ".globl ", id, "\n",
                     "  ", id, ": .word 0\n"]
emitLiteral _
  = return []

emitBuiltinString :: Identifier -> CodeGenerator String
emitBuiltinString "__builtin_Read_TChar"
  = emitString "__builtin_fmt_read_char" "\" %c\\0\""

emitBuiltinString "__builtin_Read_TInt"
  = emitString "__builtin_fmt_read_int" "\"%d\\0\""

emitBuiltinString "__builtin_Print_TInt"
  = emitString "__builtin_fmt_int" "\"%d\\0\""

emitBuiltinString "__builtin_Print_TBool"
  = liftM2 (++)
      (emitString "__builtin_str_true" "\"true\"")
      (emitString "__builtin_str_false" "\"false\"")

emitBuiltinString "__builtin_Print_TString"
  = emitString "__builtin_fmt_string" "\"%.*s\\0\""

emitBuiltinString "__builtin_Print_TRef"
  = emitString "__builtin_fmt_ref" "\"%p\\0\""

emitBuiltinString "__builtin_PrintLn"
  = emitString "__builtin_str_newline" "\"\\0\""

emitBuiltinString _
  = return []

emitLiterals :: Code -> CodeGenerator String
emitLiterals = liftM concat . mapM emitLiteral

emitBuiltinStrings :: Set Identifier -> CodeGenerator String
emitBuiltinStrings = liftM concat . mapM emitBuiltinString . Set.toList
