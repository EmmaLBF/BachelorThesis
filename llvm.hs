-- Requires: llvm-hs-pure, llvm-hs, and LLVM installed on your system.

import qualified NamedLang as NL

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

llvm :: NamedLang a -> String
llvm expr = go [] expr
  where
    -- env maps variable index -> name
    go :: [(Int, String)] -> NamedLang t -> String
    go env e =
      case e of
        Var x ->
          case lookup x env of
            Just name -> name
            Nothing   -> "x" ++ show x

        Lam f ->
          -- find next unused Int for naming
          let x = nextFree env
              name = "x" ++ show x
              env' = (x, name) : env
              body = f x  -- **use the same Int** translate would have used
          in "(\\" ++ name ++ " ->\n\t " ++ go env' body ++ ")"

        Apply f a ->
          "(" ++ go env f ++ " " ++ go env a ++ ")"

        Fix f ->
          "(fix " ++ go env f ++ ")"

        If cond t e ->
          "(if " ++ go env cond
          ++ "\n\tthen " ++ go env t
          ++ "\n\telse " ++ go env e ++ ")"

        LInt n -> show n
        LBool b -> show b
        LIntOp op l r -> "(" ++ go env l ++ " " ++ show op ++ " " ++ go env r ++ ")"
        LCmpOp op l r -> "(" ++ go env l ++ " " ++ show op ++ " " ++ go env r ++ ")"
        Prod a b -> "(" ++ go env a ++ ", " ++ go env b ++ ")"
        Fst p -> "(fst " ++ go env p ++ ")"
        Snd p -> "(snd " ++ go env p ++ ")"

-- pick next free integer not used in env
nextFree :: [(Int, String)] -> Int
nextFree env = case env of
  [] -> 0
  xs -> 1 + maximum (map fst xs)

