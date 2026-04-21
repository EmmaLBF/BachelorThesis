{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Avoid lambda" #-}
{- HLINT ignore "Use first" -}

module C where

import AbsLang (BinOp(..), CmpOp(..))
import CLang2 (CExpression, CValue, indentStr, showCExpression, showProx)
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang2 as CL

import Data.Dynamic
import Control.Monad.State
import Data.Typeable
import Debug.Trace
import System.IO

data CParam where
  CParam :: Typeable a => Int -> Proxy a -> CParam

type CParams = [CParam]

data CStatement where
  BindExpr :: Typeable a => CExpression a -> Int -> CStatement -> CStatement
  Seq :: CStatement -> CStatement -> CStatement
  If :: CExpression Bool -> CStatement -> CStatement -> CStatement
  -- DefFun :: (Typeable a, Typeable b) => Proxy a -> Int -> Int -> CStatement -> CExpression b -> CStatement
  DefFun    :: (Typeable b) 
            => Proxy b      -- return type
            -> Int          -- function id
            -> CParams
            -> CStatement   -- body setup
            -> CExpression b -- body result
            -> CStatement
  DefVar :: Typeable a => Int -> CExpression a -> CStatement
  UpdateVar :: Typeable a => Int -> CExpression a -> CStatement
  While :: CExpression Bool -> CStatement -> CStatement
  Skip :: CStatement

translate :: CL.CStatement -> CStatement
translate (CL.BindExpr x i s) =
    let s' = translate s
    in BindExpr x i s'
translate CL.Skip = Skip
translate (CL.While cond x) = 
    let x' = translate x
    in While cond x'
translate (CL.UpdateVar i x) = UpdateVar i x
translate (CL.DefVar i x) = DefVar i x
translate (CL.If cond x y) =
    let x' = translate x
        y' = translate y
    in If cond x' y'
translate (CL.Seq x y) =
    let x' = translate x
        y' = translate y
    in Seq x' y'
-- translate (CL.DefFun tret ifun (iparam, tparam) (CL.DefFun tret1 ifun1 (iparam1, tparam1) body1 ret1) ret) =
--     let body1' = translate body1
    -- in DefFun tret1 ifun [CParam iparam tparam, CParam iparam1 tparam1] body1' ret1
translate (CL.DefFun tret ifun (iparam, tparam) body ret) =
    let body' = translate body
    in DefFun tret ifun [CParam iparam tparam] body' ret

--   evalStmt (DefFunMultiArg (_ :: Proxy a) ifun iparams bodySetup (bodyResult :: CExpression b)) m =
--   let fn = mkFn iparams m
--       m' = Extend ifun fn m
--   in trace ("hello = " ++ show (typeRep fn)) $ m'
--   where
--     mkFn :: [Int] -> Env -> CValue a
--     mkFn [] env =
--       let env' = evalStmt bodySetup env
--       in unsafeCoerce $ evalExpr bodyResult env'
--     mkFn (i:is) env = unsafeCoerce $ FunV $ \(arg :: CValue a) -> mkFn is (Extend i arg env)


-- lambdalift :: CStatement -> CStatement
-- lambdalift (DefFun prox ifun iparam inner@(DefFun {}) arg) =
--   case lambdalift inner of
--     DefFunMultiArg _ _ iparams body res -> DefFunMultiArg prox ifun (iparam:iparams) body res
--     other -> DefFunMultiArg prox ifun [iparam] other arg
-- lambdalift (Seq x y) = Seq (lambdalift x) (lambdalift y)
-- lambdalift x = x


-- showCStmt indent (DefFunMultiArg prox ifun iparams stup res) =
--     "\n" ++ indentStr indent ++ show (typeRep prox) ++ " function" ++ show ifun ++ " (" ++ showParams iparams ++ ") {"
--     ++ showCStmt (indent + 1) m stup
--     ++ "\n" ++ indentStr (indent + 1) ++ "return " ++ showCExpression res ++ ";"
--     ++ "\n" ++ indentStr indent ++ "}"
--     where
--       showParams :: [Int] -> String
--       showParams [] = ""
--       showParams [i] = 
--         case lookupEnvType i of
--           Nothing -> error $ "variable not found in print " ++ show i
--           Just v -> show v ++ " v" ++ show i
--       showParams (i:is) = showParams [i] ++ ", " ++ showParams is

showCParams :: CParams -> String
showCParams [] = ""
showCParams [CParam i t] = showProx (typeRep t) ++ " v" ++ show i
showCParams (i:is) = showCParams [i] ++ ", " ++ showCParams is

showCStmt :: Int -> CStatement -> String
showCStmt indent (UpdateVar i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x ++ ";"
showCStmt indent (If cond t f) =
    "\n" ++ indentStr indent ++ "if " ++ showCExpression cond ++ " {"
    ++  showCStmt (indent + 1) t
    ++ "\n" ++ indentStr indent ++ "} else {"
    ++ showCStmt (indent + 1) f
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond ++ " {"
    ++ showCStmt (indent + 1) body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (BindExpr x i y) =
    "\n" ++ indentStr indent ++ "let v" ++ show i ++ " = " ++ showCExpression x ++ " in"
    ++ showCStmt (indent + 1) y
showCStmt indent (Seq x y) =
    showCStmt indent x ++ showCStmt indent y
showCStmt indent (DefFun prox ifun params stup res) =
    "\n" ++ indentStr indent ++ showProx (typeRep prox) ++ " v" ++ show ifun ++ "(" ++ showCParams params ++ ") {"
    ++ showCStmt (indent + 1) stup
    ++ "\n" ++ indentStr (indent + 1) ++ "return " ++ showCExpression res ++ ";"
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (DefVar i f) =  "\n" ++ indentStr indent ++ showProx (typeRep f) ++ " v" ++ show i ++ " = " ++ showCExpression f ++ ";"
showCStmt _ Skip = ""


main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.fac
        cl = evalState (CL.translate nl) c'
        c = translate (CL.setup cl)
    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 (CL.setup cl)
    putStrLn $ "return " ++ showCExpression (CL.result cl) ++ ";"
    putStrLn "\n--- Translating to C ---"
    let fileName = "output.c"
    let headers = "#include <stdbool.h>\n" ++ "typedef int (*intToint)(int);\n"
    let body = showCStmt 0 c
    let ret = "\nreturn " ++ CL.showCExpression (CL.result cl) ++ ";\n"
    let content  = headers ++ body ++ ret
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName