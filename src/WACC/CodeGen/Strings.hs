{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.Strings where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

emitString :: Identifier -> String -> CodeGenerator ()
emitString id str = do
  tell ["  ", id, ":\n"]
  tell ["    ", ".word ", show . length $ str, "\n"]
  tell ["    ", ".asciz \"", str, "\"\n"]

emitStringLiteral :: Instruction -> CodeGenerator ()
emitStringLiteral (Special (StringLit id str))
  = emitString id str
emitStringLiteral _
  = skip

emitLiterals :: Code -> CodeGenerator ()
emitLiterals = mapM_ emitStringLiteral


emitBuiltinString :: Instruction -> CodeGenerator ()
emitBuiltinString (Load _ _ (Label "__builtin_fmt_int") _ _)
  = emitString "__builtint_fmt_int" "%d"

emitBuiltinString (Load _ _ (Label "__builtin_fmt_string") _ _)
  = emitString "__builtint_fmt_string" "%.*s\0"

emitBuiltinString (Load _ _ (Label "__builtin_str_true") _ _)
  = emitString "__builtin_str_true" "true"

emitBuiltinString (Load _ _ (Label "__builtin_str_false") _ _)
  = emitString "__builtin_str_false" "false"

emitBuiltinString (Load _ _ (Label "__builtin_fmt_read_char") _ _)
  = emitString "__builtin_fmt_read_char" " %c\0"

emitBuiltinString (Load _ _ (Label "__builtin_fmt_read_int") _ _)
  = emitString "__builtin_fmt_read_int" " %d\0"

emitBuiltinString _
  = skip


emitBuiltinStrings :: Code -> CodeGenerator ()
emitBuiltinStrings = mapM_ emitBuiltinString
