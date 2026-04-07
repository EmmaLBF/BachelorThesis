-- Requires: llvm-hs-pure, llvm-hs, and LLVM installed on your system.

module LLVMLang where

import qualified AbsLang as AL
import qualified FirstOrderLang as FOL

import qualified LLVM.AST as LLVM hiding (function)
import qualified LLVM.AST.Type as LLVM
import qualified LLVM.AST.IntegerPredicate as LLVM
import qualified LLVM.IRBuilder.Constant as LLVM
import qualified LLVM.IRBuilder.Instruction as LLVM
import qualified LLVM.IRBuilder.Module as LLVM
import qualified LLVM.IRBuilder.Monad as LLVM

import Data.String (fromString)


-- Create a simple LLVM module
-- simpleModule :: String -> [Definition] -> AST.Module
-- simpleModule name defs = defaultModule { moduleName = fromString name, moduleDefinitions = defs }

-- Define: int main() { return 42; }
-- mainFunction :: Definition
-- mainFunction =
--     GlobalDefinition functionDefaults
--         { name        = Name "main"
--         , parameters  = ([], False)
--         , returnType  = i32
--         , basicBlocks = [basicBlock]
--         }

-- -- Basic block returning constant 42
-- basicBlock :: BasicBlock
-- basicBlock = BasicBlock
--     (Name "entry")
--     []
--     (Do $ Ret (Just (ConstantOperand (C.Int 32 42))) [])

intToLLVM :: Int -> LLVM.Operand
intToLLVM i = LLVM.int32 (fromIntegral i)

boolToLLVM :: Bool -> LLVM.Operand
boolToLLVM i = LLVM.int32 (fromIntegral (fromEnum i))

foexprToLLVM :: (LLVM.MonadIRBuilder m, LLVM.MonadModuleBuilder m) => FOL.FirstOrderExpr -> m LLVM.Operand
foexprToLLVM (FOL.LInt i) = return (intToLLVM i)
foexprToLLVM (FOL.LBool i) = return (boolToLLVM i)
foexprToLLVM (FOL.LIntOp o l r) = do 
    lhs <- foexprToLLVM l         
    rhs <- foexprToLLVM r
    case o of
        AL.Min -> LLVM.sub lhs rhs
        AL.Plus -> LLVM.add lhs rhs
        AL.Div -> LLVM.sdiv lhs rhs
        AL.Times -> LLVM.mul lhs rhs
        AL.Mod -> LLVM.srem lhs rhs
        
foexprToLLVM (FOL.LCmpOp o l r) = do
    lhs <- foexprToLLVM l
    rhs <- foexprToLLVM r
    case o of
        AL.Lt -> LLVM.icmp LLVM.SLT lhs rhs
        AL.Eq -> LLVM.icmp LLVM.EQ lhs rhs
        AL.Gt -> LLVM.icmp LLVM.SGT lhs rhs

foexprToLLVM (FOL.If cond a b) = do
    thenBlock <- LLVM.block `LLVM.named` "then"
    elseBlock <- LLVM.block `LLVM.named` "else"
    return (
        LLVM.condBr (foexprToLLVM cond) thenBlock elseBlock
        LLVM.ret a
        LLVM.ret b)

foexprToLLVM (FOL.App f a) = intToLLVM 1
foexprToLLVM (FOL.Lam i b) = intToLLVM 1
foexprToLLVM (FOL.Fix i f) = intToLLVM 2
foexprToLLVM (FOL.Var x) = intToLLVM 3 -- need some contx. map here
-- alloca for each variable
-- set its value and load it

foexprToLLVM (FOL.Prod a b) = intToLLVM 3 -- alloca but for an array with two items?
foexprToLLVM (FOL.Fst x) = intToLLVM 3  -- getelementptr to first element and load it
foexprToLLVM (FOL.Snd x) = intToLLVM 3  -- getelementptr to second element and load it

