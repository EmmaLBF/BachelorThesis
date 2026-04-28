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

import CLang (CExpression(..), indentStr, showCExpression, showProx)
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang as CL

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
translate (CL.DefFun _ _ (iparam, tparam) (CL.DefFun tret1 ifun1 (iparam1, tparam1) body1)) =
    DefFun tret1 ifun1 [CParam iparam tparam, CParam iparam1 tparam1] (translate body1)
translate (CL.DefFun tret ifun (iparam, tparam) body) =
    DefFun tret ifun [CParam iparam tparam] (translate body)
translate (CL.Return (x :: CExpression a)) = Return x

paramsToSet :: CParams -> Set.Set Int
paramsToSet [] = Set.empty
paramsToSet [CParam i _] = Set.singleton i
paramsToSet (i:is) = Set.union (paramsToSet [i]) (paramsToSet is)

paramsToMap :: CParams -> Map Int CParam
paramsToMap = Map.fromList . Prelude.map (\p@(CParam i _) -> (i, p))

merge :: (Map Int CParam, Map Int CParam) -> (Map Int CParam, Map Int CParam) -> (Map Int CParam, Map Int CParam)
merge (xfree, xbound) (yfree, ybound) = (Map.union xfree yfree, Map.union xbound ybound)

-- free, bound
freeVarsExpr :: forall a. CExpression a -> (Map Int CParam, Map Int CParam)
freeVarsExpr (Var i) = (Map.singleton i (CParam i (Proxy :: Proxy a)), Map.empty)
freeVarsExpr (Val _) = (Map.empty, Map.empty)
freeVarsExpr (Not x) = freeVarsExpr x
freeVarsExpr (LIntOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LCmpOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (Prod x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (Fst x) = freeVarsExpr x
freeVarsExpr (Snd x) = freeVarsExpr x
freeVarsExpr (CallExpr f x) = merge (freeVarsExpr f) (freeVarsExpr x)
freeVarsExpr (Ternary cond thn els) = merge (merge (freeVarsExpr cond) (freeVarsExpr thn)) (freeVarsExpr els)
freeVarsExpr (ConsList l x) = merge (freeVarsExpr l) (freeVarsExpr x)
freeVarsExpr (TailList l) = freeVarsExpr l
freeVarsExpr (HeadList l) = freeVarsExpr l
freeVarsExpr (IsEmpty l) = freeVarsExpr l
freeVarsExpr (IndexList l _) = freeVarsExpr l
freeVarsExpr EmptyList = (Map.empty, Map.empty)

-- free, bound
freeVarsStmt :: Typeable a => CStatement a -> (Map Int CParam, Map Int CParam)
freeVarsStmt (BindExpr (x :: CExpression a) i y) =
    let (mfree, mbound) = merge (freeVarsExpr x) (freeVarsStmt y)
    in (mfree, Map.insert i (CParam i (Proxy :: Proxy a)) mbound)
freeVarsStmt (Seq x y) = merge (freeVarsStmt x) (freeVarsStmt y)
freeVarsStmt (If cond x y) = merge (freeVarsExpr cond) (merge (freeVarsStmt x) (freeVarsStmt y))
freeVarsStmt (While cond x) = merge (freeVarsExpr cond) (freeVarsStmt x)
freeVarsStmt (DefFun _ ifun params body) =
    let (bfree, bbound) = freeVarsStmt body
        boundKeys = paramsToMap params
    in (bfree, Map.insert ifun undefined (Map.union bbound boundKeys))
freeVarsStmt (UpdateVar i (x :: CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (Map.union (Map.singleton i (CParam i (Proxy :: Proxy a))) xfree, xbound)
freeVarsStmt (DefVar i (x :: CExpression a)) =
    let (xfree, xbound) = freeVarsExpr x
    in (xfree, Map.insert i (CParam i (Proxy :: Proxy a)) xbound)
freeVarsStmt (Return x) = freeVarsExpr x
freeVarsStmt Skip = (Map.empty, Map.empty)

freeVars :: Typeable a => CStatement a -> Map Int CParam
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
rewriteExpr m  (CallExpr f x) = CallExpr (rewriteExpr m f) (rewriteExpr m x)
rewriteExpr m  (Not x) = Not (rewriteExpr m x)
rewriteExpr m  (LIntOp op x y) = LIntOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (LCmpOp op x y) = LCmpOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (Prod x y) = Prod (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (Fst x) = Fst (rewriteExpr m x)
rewriteExpr m  (Snd x) = Snd (rewriteExpr m x)
rewriteExpr m  (IsEmpty x) = IsEmpty (rewriteExpr m x)
rewriteExpr m  (HeadList x) = HeadList (rewriteExpr m x)
rewriteExpr m  (TailList x) = TailList (rewriteExpr m x)
rewriteExpr m  (ConsList l x) = ConsList (rewriteExpr m l) (rewriteExpr m x)
rewriteExpr m  (IndexList l i) = IndexList (rewriteExpr m l) i
rewriteExpr _  EmptyList = EmptyList
rewriteExpr m (Ternary x y z) = Ternary (rewriteExpr m x) (rewriteExpr m y) (rewriteExpr m z)
rewriteExpr _ x = x

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
rewriteStmt m (Return x) = Return (rewriteExpr m x)
rewriteStmt _ Skip = Skip

type Lifted a = [CStatement a]

liftedFunsList :: Lifted a -> [Int]
liftedFunsList [] = []
liftedFunsList [DefFun _ i _ _] = [i]
liftedFunsList (i:is) = liftedFunsList [i] ++ liftedFunsList is

liftStmt :: LiftEnv -> [Int] -> CStatement a -> (LiftEnv, Lifted a, CStatement a)
liftStmt env funs (DefFun tret ifun params body) =
    let freeMapRaw        = freeVars (DefFun tret ifun params body)
        freeMap = Map.withoutKeys freeMapRaw (Set.fromList funs)
        extraPs        = Map.elems freeMap
        newParams      = params ++ extraPs
        env'           = Map.insert ifun extraPs env
        (env'', lifted, body') = liftStmt env' (funs ++ [ifun]) body
        body''         = rewriteStmt env'' body'
        thisDef        = DefFun tret ifun newParams body''
    in (env'', lifted ++ [thisDef], Skip)  -- replace with Skip, float definition out
liftStmt env funs (Seq x y) =
    let (env',  lx, x') = liftStmt env funs  x
        (env'', ly, y') = liftStmt env' funs y
    in (env'', lx ++ ly, Seq x' y')
liftStmt env funs (If cond x y) =
    let (env',  lx, x') = liftStmt env funs  x
        (env'', ly, y') = liftStmt env' funs y
    in (env'', lx ++ ly, If (rewriteExpr env cond) x' y')
liftStmt env funs (While cond x) =
    let (env', lx, x') = liftStmt env funs x
    in (env', lx, While (rewriteExpr env cond) x')
liftStmt env funs (BindExpr x i y) =
    let (env', ly, y') = liftStmt env funs y
    in (env', ly, BindExpr (rewriteExpr env x) i y')
liftStmt env _ s = (env, [], rewriteStmt env s)

lambdaLift :: CStatement a -> (CStatement a, [CStatement a])
lambdaLift stmt =
    let (_, lifted, stmt') = liftStmt Map.empty [] stmt
    in (Prelude.foldr Seq stmt' lifted, lifted)

showProxVar :: String -> TypeRep -> String
showProxVar s p =
    let args = typeRepArgs p
        con  = show (typeRepTyCon p)
    in case (con, args) of
        ("Int",  [])     -> "int " ++ s
        ("Bool", [])     -> "bool " ++ s
        ("()",   [])     -> "void* " ++ s
        ("[]",   [_])     -> "Node* " ++ s
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

listPreamble :: String
listPreamble =
    "\n// List Definitions" ++
    "\ntypedef struct Node {" ++
    "\n    void* head;" ++
    "\n    struct Node* tail;" ++
    "\n} Node;\n" ++
    "\nNode* cons(void* head, Node* tail) {" ++
    "\n    Node* node = malloc(sizeof(Node));" ++
    "\n    node->head = head;" ++
    "\n    node->tail = tail;" ++
    "\n    return node;" ++ "\n}" ++ "\n" ++
    "\nint isEmpty(Node* xs) {" ++
    "\n    return xs == NULL;" ++
    "\n}\n" ++
    "\nvoid* head(Node* xs) {" ++
    "\n    return xs->head;" ++
    "\n}\n" ++
    "\nNode* tail(Node* xs) {" ++
    "\n    return xs->tail;\n}\n"

usesListExpr :: CExpression a -> Bool
usesListExpr EmptyList = True
usesListExpr (ConsList _ _) = True
usesListExpr (IsEmpty _ )= True
usesListExpr HeadList {} = True
usesListExpr TailList {} = True
usesListExpr (Val (CL.ListV _)) = True
usesListExpr (CallExpr f x) = usesListExpr f || usesListExpr x
usesListExpr _ = False

usesList :: CStatement a -> Bool
usesList (Return x) = usesListExpr x
usesList (BindExpr x _ y) = usesListExpr x || usesList y
usesList (Seq x y) = usesList x || usesList y
usesList (If x y z) = usesListExpr x || usesList y || usesList z
usesList (While x y) = usesListExpr x || usesList y
usesList (UpdateVar _ x) = usesListExpr x
usesList (DefVar _ x) = usesListExpr x
usesList (DefFun _ _ _ y) = usesList y
usesList Skip = False

findFirstReturn :: CStatement a -> CExpression a
findFirstReturn (Return x) = x
findFirstReturn (Seq x y) =
    case x of
        (Return i) -> i
        _ -> findFirstReturn y
findFirstReturn (BindExpr _ _ y) = findFirstReturn y
findFirstReturn _ = error "no return"


removeFirstReturn :: CStatement a -> CStatement a
removeFirstReturn (Return _) = Skip
removeFirstReturn (Seq (Return _) y) = Seq Skip y
removeFirstReturn (Seq x (Return _)) = Seq x Skip
removeFirstReturn (Seq x y) = Seq (removeFirstReturn x) (removeFirstReturn y)
removeFirstReturn x = x

makeFunDefs :: [CStatement a] -> String
makeFunDefs [] = ""
makeFunDefs [DefFun tret ifun params _] = "\n" ++ showProxFunc ("v" ++ show ifun) params (typeRep tret) ++ ";"
makeFunDefs (i:is) = makeFunDefs[i] ++ makeFunDefs is

main :: IO ()
main = do
    let progName = "sumListCall"
    let (nl, c') = NL.translate 0 AL.sumListCall
        cl = evalState (CL.translate nl) c'
        c = translate cl

    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 cl

    putStrLn "\n--- Printing C ---"
    let (cbody, defs) = lambdaLift c
    let imports = "// imports" ++
                "\n#include <stdbool.h>" ++
                "\n#include <stdio.h>" ++
                "\n#include <stdlib.h>\n"
    let funDefs = makeFunDefs defs
    let ret = findFirstReturn cbody
    let bodyWithoutRet = removeFirstReturn cbody
    let body = showCStmt 0 bodyWithoutRet ++ "\nint main(void) {\n" ++
                    "  printf(\"%d\\n\", " ++ showCExpression ret ++ ");\n" ++
                    "  return 0;\n}\n"
    let contentHeader = if usesList cbody then imports ++ listPreamble else imports
    let content = contentHeader ++ "\n// Function Definitions" ++ funDefs ++ "\n\n// Compiled Program" ++ body

    -- writing to file
    let fileName = "outputs/" ++ progName ++ "_output.c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName