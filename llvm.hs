import NamedLang as NL

-- Requires: llvm-hs-pure, llvm-hs, and LLVM installed on your system.

import LLVM.AST
import LLVM.AST.Global
import LLVM.AST.Type as AST
import LLVM.AST.Constant as C
import LLVM.AST.AddrSpace
import qualified LLVM.AST as AST
import qualified LLVM.Pretty as Pretty

import Data.String (fromString)

main :: IO ()
main = do
    let astModule = simpleModule "myModule" [mainFunction]
    putStrLn (show (Pretty.ppll astModule))

-- Create a simple LLVM module
simpleModule :: String -> [Definition] -> AST.Module
simpleModule name defs = defaultModule { moduleName = fromString name, moduleDefinitions = defs }

-- Define: int main() { return 42; }
mainFunction :: Definition
mainFunction =
    GlobalDefinition functionDefaults
        { name        = Name "main"
        , parameters  = ([], False)
        , returnType  = i32
        , basicBlocks = [basicBlock]
        }

-- Basic block returning constant 42
basicBlock :: BasicBlock
basicBlock = BasicBlock
    (Name "entry")
    []
    (Do $ Ret (Just (ConstantOperand (C.Int 32 42))) [])

