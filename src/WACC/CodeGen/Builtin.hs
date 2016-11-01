module WACC.CodeGen.Builtin where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Types

-- #COPY#
-- data BuiltinFunc
--   = Read
--   | Free
--   | Exit
--   | Print
--   | PrintLn
--   deriving (Eq, Show)

-- FIXME: arg needs to be checked to be in bounds of -255 < arg < 255
generateBuiltin :: BuiltinFunc -> Expr -> InstructionGenerator ()
generateBuiltin Exit (Lit (INT arg)) = do
  tell [PureAsm ["mov r0, #",show arg, "\n"]]
  tell [PureAsm ["mov r7, #1\n", "swi #0\n"]]

emitBuiltin Exit _ = undefined

emitBuiltin _ _ = return ()
