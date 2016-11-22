{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.Strings where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

emitString :: Identifier -> String -> CodeGenerator String
emitString id str
  = return $ concat ["  ", id, ":\n",
                     "    ", ".word ", show . length $ str, "\n",
                     "    ", ".asciz ", str, "\n"]

emitStringLiteral :: Instruction -> CodeGenerator String
emitStringLiteral (Special (StringLit id str))
  = emitString id (show str)
emitStringLiteral _
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

emitBuiltinString "__builtin_PrintLn"
  = emitString "__builtin_str_newline" "\"\\0\""

emitBuiltinString _
  = return []

emitLiterals :: Code -> CodeGenerator String
emitLiterals = liftM concat . mapM emitStringLiteral

emitBuiltinStrings :: Set Identifier -> CodeGenerator String
emitBuiltinStrings = liftM concat . mapM emitBuiltinString . Set.toList
