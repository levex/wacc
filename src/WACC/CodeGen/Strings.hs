{-# LANGUAGE RecordWildCards #-}
module WACC.CodeGen.Strings where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

emitStringLiteral :: Instruction -> CodeGenerator ()
emitStringLiteral (Special (StringLit id str)) = do
  tell ["  ", id, ":\n"]
  tell ["    ", ".word ", show . length $ str, "\n"]
  tell ["    ", ".asciz \"", str, "\"\n"]
emitStringLiteral _
  = skip

emitLiterals :: Code -> CodeGenerator ()
emitLiterals p = do
  tell [".section \".data\"\n"]
  mapM_ emitStringLiteral p
