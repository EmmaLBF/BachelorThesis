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
import Data.List
import Data.Char
import Data.Set
import qualified Data.Set as Set
import Data.Map
import qualified Data.Map as Map
import Unsafe.Coerce
import GHC.Exts

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
translate (CL.BindExpr x i s) = BindExpr x i (translate s)
translate CL.Skip = Skip
translate (CL.While cond x) = While cond (translate x)
translate (CL.UpdateVar i x) = UpdateVar i x
translate (CL.DefVar i x) = DefVar i x
translate (CL.If cond x y) = If cond (translate x) (translate y)
translate (CL.Seq x y) = Seq (translate x) (translate y)
translate (CL.DefFun tret ifun (iparam, tparam) body ret) =
    DefFun tret ifun [CParam iparam tparam] (translate body) ret
-- translate (CL.DefFun tret ifun (iparam, tparam) (CL.DefFun tret1 ifun1 (iparam1, tparam1) body1 ret1) ret) =
--     let body1' = translate body1
--     in DefFun tret1 ifun [CParam iparam tparam, CParam iparam1 tparam1] body1' ret1
-- translate (CL.DefFun tret ifun (iparam, tparam) body ret) =
--   let body' = translate body
--   in case body' of
--     DefFun tret1 ifun1 params1 body1 ret1 ->
--       DefFun tret1 ifun (CParam iparam tparam : params1) body1 ret1
--     _ ->
--       DefFun tret ifun [CParam iparam tparam] body' ret

-- takes list of free vars and cstmt
-- lambdaLift :: CParams -> CStatement -> [CStatement]
-- lambdaLift [] (DefFun tret ifun params body (ret :: CExpression b)) =
--     lambdaLift params body ++ [DefFun tret ifun params body ret]
-- lambdaLift [CParam i t] (DefFun tret ifun params body ret) =
--     let newParams = params ++ [CParam i t]
--         body' = body
--         ret' = ret
--     in lambdaLift newParams body' ++ [DefFun tret ifun newParams body' ret']
-- lambdaLift l (Seq x y) = lambdaLift l x ++ lambdaLift l y
-- lambdaLift [] _ = []
-- lambdaLift _ _ = []

paramsToSet :: CParams -> Set Int
paramsToSet [] = Set.empty
paramsToSet [CParam i _] = Set.singleton i
paramsToSet (i:is) = Set.union (paramsToSet [i]) (paramsToSet is)

paramsToMap :: CParams -> Map Int CParam
paramsToMap = Map.fromList . Prelude.map (\p@(CParam i _) -> (i, p))

-- free, bound
freeVarsExpr :: forall a. CExpression a -> (Map Int CParam, Map Int CParam)
freeVarsExpr (CL.Var i) = (Map.singleton i (CParam i (Proxy :: Proxy a)), Map.empty)
freeVarsExpr (CL.Val _) = (Map.empty, Map.empty)
freeVarsExpr (CL.Not x) = freeVarsExpr x
freeVarsExpr (CL.LIntOp _ x y) = 
    let (xfree, xbound) = freeVarsExpr x
        (yfree, ybound) = freeVarsExpr y
    in (Map.union xfree yfree, Map.union xbound ybound)
freeVarsExpr (CL.LCmpOp _ x y) =
    let (xfree, xbound) = freeVarsExpr x
        (yfree, ybound) = freeVarsExpr y
    in (Map.union xfree yfree, Map.union xbound ybound)
freeVarsExpr (CL.Prod x y) =
    let (xfree, xbound) = freeVarsExpr x
        (yfree, ybound) = freeVarsExpr y
    in (Map.union xfree yfree, Map.union xbound ybound)
freeVarsExpr (CL.Fst x) = freeVarsExpr x
freeVarsExpr (CL.Snd x) = freeVarsExpr x
freeVarsExpr (CL.CallExpr f x) =
    let (ffree, fbound) = freeVarsExpr f
        (xfree, xbound) = freeVarsExpr x
    in (Map.union xfree ffree, Map.union xbound fbound)

-- free, bound
freeVarsStmt :: CStatement -> (Map Int CParam, Map Int CParam)
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
freeVarsStmt (DefFun _ ifun params body ret) =
    let (bfree, bbound) = freeVarsStmt body
        (rfree, rbound) = freeVarsExpr ret
        usedVars  = Map.union bfree rfree
        boundKeys = paramsToMap params
        allbound  = Map.union bbound rbound
    --in ( Map.withoutKeys usedVars (Set.insert ifun (Map.keysSet boundKeys))
    in ( Map.withoutKeys usedVars (Set.insert ifun (Map.keysSet boundKeys))
       , Map.insert ifun undefined (Map.union allbound boundKeys))
freeVarsStmt (UpdateVar i x) =
    let (xfree, xbound) = freeVarsExpr x
    in (Map.union (Map.singleton i (CParam i (Proxy :: Proxy Int))) xfree, xbound)
freeVarsStmt (DefVar i (x :: CL.CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (xfree, Map.insert i (CParam i (Proxy :: Proxy a)) xbound)
freeVarsStmt Skip = (Map.empty, Map.empty)

freeVars :: CStatement -> Map Int CParam
freeVars s = 
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

applyArgs :: forall a. Typeable a => CExpression a -> CParams -> CExpression a
applyArgs acc [] = acc
applyArgs acc ((CParam i (Proxy :: Proxy p)) : vs) =
    let applied = CL.CallExpr
                    (unsafeCoerce acc :: CL.CExpression (p -> a))
                    (CL.Var i :: CL.CExpression p)
    in applyArgs (unsafeCoerce applied) vs


type LiftEnv = Map Int CParams  -- function id → extra param variable ids
-- Map.insertWith (++) f arg m

rewriteExpr :: LiftEnv -> CExpression a -> CExpression a
rewriteExpr env (CL.CallExpr (CL.Var f) x) =
  let x' = rewriteExpr env x
      base = CL.CallExpr (CL.Var f) x'
  in case Map.lookup f env of
       Just extraVars -> applyArgs base extraVars
       Nothing -> base
rewriteExpr m  (CL.Not x) = CL.Not (rewriteExpr m x)
rewriteExpr m  (CL.LIntOp op x y) = CL.LIntOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (CL.LCmpOp op x y) = CL.LCmpOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (CL.Prod x y) = CL.Prod (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (CL.Fst x) = CL.Fst (rewriteExpr m x)
rewriteExpr m  (CL.Snd x) = CL.Snd (rewriteExpr m x)
rewriteExpr m  x = x

rewriteStmt :: LiftEnv -> CStatement -> CStatement 
rewriteStmt m (BindExpr x i y) = BindExpr (rewriteExpr m x) i (rewriteStmt m y)
rewriteStmt m (Seq x y) = Seq (rewriteStmt m x) (rewriteStmt m y)
rewriteStmt m (If cond x y) = If (rewriteExpr m cond) (rewriteStmt m x) (rewriteStmt m y)
rewriteStmt m (While cond x) = While (rewriteExpr m cond) (rewriteStmt m x)
rewriteStmt m (DefFun tret ifun params body ret) = 
    DefFun tret ifun params
    (rewriteStmt m body)
    (rewriteExpr m ret)
rewriteStmt m (UpdateVar i x) = UpdateVar i (rewriteExpr m x)
rewriteStmt m (DefVar i x) = DefVar i (rewriteExpr m x)
rewriteStmt _ x = x

type Lifted = [CStatement]

liftStmt :: LiftEnv -> CStatement -> (LiftEnv, Lifted, CStatement)
liftStmt env (DefFun tret ifun params body ret) =
    let freeMap        = freeVars (DefFun tret ifun params body ret)
        extraPs        = Map.elems freeMap
        newParams      = params ++ extraPs
        env'           = Map.insert ifun extraPs env
        (env'', lifted, body') = liftStmt env' body
        body''         = rewriteStmt env'' body'
        ret'           = rewriteExpr env'' ret
        thisDef        = DefFun tret ifun newParams body'' ret'
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

lambdaLift :: CStatement -> CStatement
lambdaLift stmt =
    let (_, lifted, stmt') = liftStmt Map.empty stmt
        -- reassemble: all lifted defs at top level, then the rewritten program
    in Prelude.foldr Seq stmt' lifted

-- replace all callexpr with correct args
-- addArgToCallsStmt :: Int -> CStatement -> CStatement
-- addArgToCallsStmt i (If cond t f) = If cond (addArgToCallsStmt i t) (addArgToCallsStmt i f)
-- addArgToCallsStmt i (Seq x y) = Seq (addArgToCallsStmt i x) (addArgToCallsStmt i y)
-- addArgToCallsStmt i (UpdateVar j x) = UpdateVar j (addArgToCallsExpr i x)
-- addArgToCallsStmt i (DefVar j x) = DefVar j (addArgToCallsExpr i x)
-- addArgToCallsStmt _ Skip = Skip
-- addArgToCallsStmt _ s = s

-- addArgToCallsExpr :: Int -> CL.CExpression a -> CL.CExpression a
-- addArgToCallsExpr i ((CL.CallExpr (CL.Var j) x) :: CL.CExpression a) | i == j =
--     CL.CallExpr (CL.CallExpr (CL.Var j) (addArgToCallsExpr i x)) (CL.Var j)
-- addArgToCallsExpr i (CL.CallExpr f x) = CL.CallExpr (addArgToCallsExpr i f) (addArgToCallsExpr i x)
-- addArgToCallsExpr i (CL.LIntOp op x y) = CL.LIntOp op (addArgToCallsExpr i x) (addArgToCallsExpr i y)
-- addArgToCallsExpr i (CL.LCmpOp op x y) = CL.LCmpOp op (addArgToCallsExpr i x) (addArgToCallsExpr i y)
-- addArgToCallsExpr _ x = x


capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

mkTypedefName :: String -> [String] -> String
mkTypedefName retType paramTypes = concatMap capitalize paramTypes ++ "To" ++ capitalize retType

showTypedef :: (String, String, [String]) -> String
showTypedef (name, retType, paramTypes) =
    "\ntypedef " ++ retType ++ " (*" ++ name ++ ")(" ++ intercalate ", " paramTypes ++ ");"

-- collectFunTypes :: [CStatement] -> [(String, String, [String])]
-- collectFunTypes stmts = nubBy (\(a,_,_) (b,_,_) -> a == b) $ concatMap collect stmts
--   where
--     collect (DefFun prox ms _ params _ _) =
--         let paramTypes = map (\(CParam _ t) -> showProx (typeRep t)) params
--             retType = case ms of
--                         Just s  -> s
--                         Nothing -> showProx (typeRep prox)
--         in [(mkTypedefName retType paramTypes, retType, paramTypes)]
--     collect _ = []

-- showTypedefs :: [CStatement] -> String
-- showTypedefs stmts = "\n// function ptr types" ++ concatMap showTypedef (collectFunTypes stmts)

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

getRetType :: Maybe String -> String -> String
getRetType (Just s) _ = s
getRetType Nothing fallback = fallback

showCFunDecs :: [CStatement] -> [CStatement] -> String
showCFunDecs _ [] = ""
showCFunDecs al (DefFun prox ifun params _ _ : rest) =
    -- let retType = getRetType ms (showProx (typeRep prox))
    "\n" ++ showProx (typeRep prox) ++ " v" ++ show ifun ++ "(" ++ showCParams params ++ ");"
    ++ showCFunDecs al rest
showCFunDecs al (_:rest) = showCFunDecs al rest

showCFunDefs :: [CStatement] -> [CStatement] -> String
showCFunDefs _ [] = ""
showCFunDefs al (DefFun prox ifun params body res : rest) =
    -- let retType = getRetType ms (showProx (typeRep prox))
    "\n" ++ showProx (typeRep prox) ++ " v" ++ show ifun ++ "(" ++ showCParams params ++ ") {"
    ++ showCStmt 1 body
    ++ "\n  return " ++ showCExpression res ++ ";"
    ++ "\n}\n"
    ++ showCFunDefs al rest
showCFunDefs al (_:rest) = showCFunDefs al rest

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
    "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params (typeRep prox) ++ " {"
    ++ showCStmt (indent + 1) stup
    ++ "\n" ++ indentStr (indent + 1) ++ "return " ++ showCExpression res ++ ";"
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent (DefVar i f) =  "\n" ++ indentStr indent ++ showProxVar ("v" ++ show i) (typeRep f) ++ " = " ++ showCExpression f ++ ";"
showCStmt _ Skip = ""

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.fac
        cl = evalState (CL.translate nl) c'
        c = translate (CL.setup cl)
    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 (CL.setup cl)
    putStrLn $ "return " ++ showCExpression (CL.result cl) ++ ";"

    putStrLn "\n--- Lambda Lift ---"
    let lifted = lambdaLift c
    -- let defs = lambdaLift [] c
    -- let freeVars = freeVarsStmt c
    -- print freeVars
    -- let content = showCStmt 0 lifted

    putStrLn "\n--- Printing C ---"
    let fileName = "output.c"

    let imports = "\n// imports\n#include <stdbool.h>\n"
    -- let types = "// type decl "
    --let body = "\n" ++ CL.showProx (typeRep (CL.result cl)) ++ " compiled() {" ++ showCStmt 1 lifted
    let body = "\n" ++ showCStmt 0 lifted
    let mn = "int main(int argc, char *argv[]) {\n" -- am I supposed to give it an arg?
    let content = imports ++ body
    -- let funDecs = "\n// function declarations" ++ showCFunDecs defs defs ++ "\n"
    -- let funDefs = "\n// function definitions" ++ showCFunDefs defs defs
    -- let ret = "\n  return " ++ CL.showCExpression (CL.result cl) ++ ";\n}\n"
    -- let mainFun = "\nint main(void) {\n  return 0;\n}"
    -- let content  = imports ++ types ++ funDecs ++ funDefs ++ body ++ ret ++ mainFun
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName