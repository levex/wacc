module WACC.CodeGen.Functions where

import           Control.Monad.State
import           Control.Monad.Writer
import           WACC.Parser.Types
import           WACC.CodeGen.Builtin
import           WACC.CodeGen.EmitARM
import           WACC.CodeGen.Types

-- emitEnterFunction :: Definition -> CodeGenerator ()
-- emitEnterFunction d = do
--   -- FIXME: calculate required stack space
--   mapM_ emitInstruction armFunctionEnter
--
-- emitExitFunction :: Definition -> CodeGenerator ()
-- emitExitFunction d = do
--   mapM_ emitInstruction armFunctionExit
--
-- emitStatement :: Statement -> CodeGenerator ()
-- emitStatement (Block xs) = mapM_ emitStatement xs
-- emitStatement (Builtin f e) = emitBuiltin f e
-- emitStatement (IdentifiedStatement st _) = emitStatement st
-- emitStatement _ = skip -- FIXME: TODO
--
-- emitFunction :: Definition -> CodeGenerator ()
-- emitFunction d@(FunDef (id, rt) st)  = do
--   tell [".globl ", id, "\n"]
--   tell [id, ":\n"]
--   emitEnterFunction d
--   emitStatement st
--   emitExitFunction d
