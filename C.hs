{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use foldl" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{- HLINT ignore "Use first" -}

module C where

import CLang2 (CExpression(..), indentStr, showCExpression, showProx)
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang2 as CL

import Data.Dynamic
import Control.Monad.State
import Data.Typeable
import Debug.Trace
import System.IO
import qualified Data.Set as Set
import Data.Map
import qualified Data.Map as Map
import Unsafe.Coerce

data CParam where
  CParam :: Typeable a => Int -> Proxy a -> CParam

type CParams = [CParam]

data CStatement a where
    Return :: CExpression a -> CStatement a
    BindExpr :: Typeable a => CExpression a -> Int -> CStatement b -> CStatement b
    Seq :: CStatement a -> CStatement a -> CStatement a
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    DefFun :: (Typeable b)
                => Proxy b 
                -> Int
                -> CParams -> CStatement b -> CStatement b
    DefVar :: Typeable a => Int -> CExpression a -> CStatement b
    UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
    While :: CExpression Bool -> CStatement a -> CStatement a
    Skip :: CStatement a

translate :: CL.CStatement a -> CStatement a
translate (CL.BindExpr x i s) = BindExpr x i (translate s)
translate CL.Skip = Skip
translate (CL.While cond x) = While cond (translate x)
translate (CL.UpdateVar i x) = UpdateVar i x
translate (CL.DefVar i x) = DefVar i x
translate (CL.If cond x y) = If cond (translate x) (translate y)
translate (CL.Seq x y) = Seq (translate x) (translate y)
translate (CL.DefFun tret ifun (iparam, tparam) body) =
    DefFun tret ifun [CParam iparam tparam] (translate body)
translate (CL.Return x) = Return x

paramsToSet :: CParams -> Set.Set Int
paramsToSet [] = Set.empty
paramsToSet [CParam i _] = Set.singleton i
paramsToSet (i:is) = Set.union (paramsToSet [i]) (paramsToSet is)

paramsToMap :: CParams -> Map Int CParam
paramsToMap = Map.fromList . Prelude.map (\p@(CParam i _) -> (i, p))

-- free, bound
freeVarsExpr :: forall a. CExpression a -> (Map Int CParam, Map Int CParam)
freeVarsExpr (Var i) = (Map.singleton i (CParam i (Proxy :: Proxy a)), Map.empty)
freeVarsExpr (Val _) = (Map.empty, Map.empty)
freeVarsExpr (Not x) = freeVarsExpr x
freeVarsExpr (LIntOp _ x y) = 
    let (xfree, xbound) = freeVarsExpr x
        (yfree, ybound) = freeVarsExpr y
    in (Map.union xfree yfree, Map.union xbound ybound)
freeVarsExpr (LCmpOp _ x y) =
    let (xfree, xbound) = freeVarsExpr x
        (yfree, ybound) = freeVarsExpr y
    in (Map.union xfree yfree, Map.union xbound ybound)
freeVarsExpr (Prod x y) =
    let (xfree, xbound) = freeVarsExpr x
        (yfree, ybound) = freeVarsExpr y
    in (Map.union xfree yfree, Map.union xbound ybound)
freeVarsExpr (Fst x) = freeVarsExpr x
freeVarsExpr (Snd x) = freeVarsExpr x
freeVarsExpr (CallExpr f x) =
    let (ffree, fbound) = freeVarsExpr f
        (xfree, xbound) = freeVarsExpr x
    in (Map.union xfree ffree, Map.union xbound fbound)

-- free, bound
freeVarsStmt :: CStatement a -> (Map Int CParam, Map Int CParam)
freeVarsStmt (BindExpr (x :: CExpression a) i y) =
    let (xfree, xbound) = freeVarsExpr x
        (yfree, ybound) = freeVarsStmt y
    in (Map.union xfree yfree, Map.insert i (CParam i (Proxy :: Proxy a)) (Map.union xbound ybound))
freeVarsStmt (Seq x y) =
    let (xfree, xbound) = freeVarsStmt x
        (yfree, ybound) = freeVarsStmt y
    in (Map.union xfree yfree, Map.union xbound ybound)
freeVarsStmt (If cond x y) =
    let (cfree, cbound) = freeVarsExpr cond
        (xfree, xbound) = freeVarsStmt x
        (yfree, ybound) = freeVarsStmt y
    in (Map.union cfree (Map.union xfree yfree), Map.union cbound (Map.union xbound ybound))
freeVarsStmt (While cond x) =
    let (cfree, cbound) = freeVarsExpr cond
        (xfree, xbound) = freeVarsStmt x
    in (Map.union cfree xfree, Map.union cbound xbound)
freeVarsStmt (DefFun _ ifun params body) =
    let (bfree, bbound) = freeVarsStmt body
        boundKeys = paramsToMap params
    in ( Map.withoutKeys bfree (Set.insert ifun (Map.keysSet boundKeys))
       , Map.insert ifun undefined (Map.union bbound boundKeys))
freeVarsStmt (UpdateVar i x) =
    let (xfree, xbound) = freeVarsExpr x
    in (Map.union (Map.singleton i (CParam i (Proxy :: Proxy Int))) xfree, xbound)
freeVarsStmt (DefVar i (x :: CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (xfree, Map.insert i (CParam i (Proxy :: Proxy a)) xbound)
freeVarsStmt (Return _) = (Map.empty, Map.empty)
freeVarsStmt Skip = (Map.empty, Map.empty)

freeVars :: CStatement a -> Map Int CParam
freeVars s = 
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

applyArgs :: forall a. Typeable a => CExpression a -> CParams -> CExpression a
applyArgs acc [] = acc
applyArgs acc ((CParam i (Proxy :: Proxy p)) : vs) =
    let applied = CallExpr
                    (unsafeCoerce acc :: CExpression (p -> a))
                    (Var i :: CExpression p)
    in applyArgs (unsafeCoerce applied) vs


type LiftEnv = Map Int CParams

rewriteExpr :: LiftEnv -> CExpression a -> CExpression a
rewriteExpr env (CallExpr (Var f) x) =
  let x' = rewriteExpr env x
      base = CallExpr (Var f) x'
  in case Map.lookup f env of
       Just extraVars -> applyArgs base extraVars
       Nothing -> base
rewriteExpr m  (Not x) = Not (rewriteExpr m x)
rewriteExpr m  (LIntOp op x y) = LIntOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (LCmpOp op x y) = LCmpOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (Prod x y) = Prod (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (Fst x) = Fst (rewriteExpr m x)
rewriteExpr m  (Snd x) = Snd (rewriteExpr m x)
rewriteExpr _  x = x

rewriteStmt :: LiftEnv -> CStatement a -> CStatement a
rewriteStmt m (BindExpr x i y) = BindExpr (rewriteExpr m x) i (rewriteStmt m y)
rewriteStmt m (Seq x y) = Seq (rewriteStmt m x) (rewriteStmt m y)
rewriteStmt m (If cond x y) = If (rewriteExpr m cond) (rewriteStmt m x) (rewriteStmt m y)
rewriteStmt m (While cond x) = While (rewriteExpr m cond) (rewriteStmt m x)
rewriteStmt m (DefFun tret ifun params body) = 
    DefFun tret ifun params
    (rewriteStmt m body)
rewriteStmt m (UpdateVar i x) = UpdateVar i (rewriteExpr m x)
rewriteStmt m (DefVar i x) = DefVar i (rewriteExpr m x)
rewriteStmt _ x = x

type Lifted a = [CStatement a]

liftStmt :: LiftEnv -> CStatement a -> (LiftEnv, Lifted a, CStatement a)
liftStmt env (DefFun tret ifun params body) =
    let freeMap        = freeVars (DefFun tret ifun params body)
        extraPs        = Map.elems freeMap
        newParams      = params ++ extraPs
        env'           = Map.insert ifun extraPs env
        (env'', lifted, body') = liftStmt env' body
        body''         = rewriteStmt env'' body'
        thisDef        = DefFun tret ifun newParams body''
    in (env'', lifted ++ [thisDef], Skip)  -- replace with Skip, float definition out
liftStmt env (Seq x y) =
    let (env',  lx, x') = liftStmt env  x
        (env'', ly, y') = liftStmt env' y
    in (env'', lx ++ ly, Seq x' y')
liftStmt env (If cond x y) =
    let (env',  lx, x') = liftStmt env  x
        (env'', ly, y') = liftStmt env' y
    in (env'', lx ++ ly, If (rewriteExpr env cond) x' y')
liftStmt env (While cond x) =
    let (env', lx, x') = liftStmt env x
    in (env', lx, While (rewriteExpr env cond) x')
liftStmt env (BindExpr x i y) =
    let (env', ly, y') = liftStmt env y
    in (env', ly, BindExpr (rewriteExpr env x) i y')
liftStmt env s = (env, [], rewriteStmt env s)

lambdaLift :: CStatement a -> CStatement a
lambdaLift stmt =
    let (_, lifted, stmt') = liftStmt Map.empty stmt
    in Prelude.foldr Seq stmt' lifted

showProxVar :: String -> TypeRep -> String
showProxVar s p = 
    let args = typeRepArgs p
        con  = show (typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int " ++ s
        ("Bool", [])     -> "bool " ++ s
        ("()",   [])     -> "void* " ++ s
        ("(,)",  [a, _]) -> showProx a ++ "* " ++ s
        ("->",   [a, b]) -> showProx b ++ " (*" ++ s ++ ")(" ++ showProx a ++ ")"
        _                -> show p ++ s

showProxFunc :: String -> CParams -> TypeRep -> String
showProxFunc s params p =
    let args = typeRepArgs p
        con  = show (typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int " ++ s ++ "(" ++ showCParams params ++ ")"
        ("Bool", [])     -> "bool " ++ s ++ "(" ++ showCParams params ++ ")"
        ("()",   [])     -> "void* " ++ s ++ "(" ++ showCParams params ++ ")"
        ("(,)",  [a, _]) -> showProx a ++ "* " ++ s ++ "(" ++ showCParams params ++ ")"
        ("->",   [a, b]) -> showProx b ++ " (*" ++ s ++ "(" ++ showCParams params ++ ")" ++ ")(" ++ showProx a ++ ")"
        _                -> show p ++ s ++ "(" ++ showCParams params ++ ")"

showCParams :: CParams -> String
showCParams [] = ""
showCParams [CParam i t] = showProxVar ("v" ++ show i) (typeRep t)
showCParams (i:is) = showCParams [i] ++ ", " ++ showCParams is

showCStmt :: Int -> CStatement a -> String
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
showCStmt indent (DefFun prox ifun params body) =
    "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params (typeRep prox) ++ " {"
    ++ showCStmt (indent + 1) body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent (DefVar i f) =  "\n" ++ indentStr indent ++ showProxVar ("v" ++ show i) (typeRep f) ++ " = " ++ showCExpression f ++ ";"
showCStmt indent (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x ++ ";"
showCStmt _ Skip = ""

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.facCall
        cl = evalState (CL.translate nl) c'
        c = translate cl
    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 cl
    putStrLn "\n--- Lambda Lift ---"
    let lifted = lambdaLift c

    putStrLn "\n--- Printing C ---"
    let fileName = "output.c"

    let imports = "// imports\n#include <stdbool.h>\n#include <stdio.h>\n"
    let body = case lifted of
                Seq f (Seq Skip (Return x)) -> 
                    showCStmt 0 f ++
                    "\nint main(void) {\n" ++
                    "  printf(\"%d\\n\", " ++ showCExpression x ++ ");\n" ++
                    "  return 0;\n}\n"
                x -> showCStmt 0 x
    let content = imports ++ body
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName