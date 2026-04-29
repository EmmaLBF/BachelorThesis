{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- gcc ./outputs/sumListCall_output.c -o ./outputs/sumListCall_output
-- ./outputs/sumListCall_output

module C where

import CLang (CExpression(..), indentStr, showProx)
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified CLang as CL

import Data.Dynamic
import Control.Monad.State
import Data.Typeable
import Debug.Trace
import System.IO
import Unsafe.Coerce
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

data CParam where
  CParam :: Typeable a => Int -> Proxy a -> CParam
type CParams = [CParam]
type CParamMap = Map.Map Int CParam

data CArg where
  CArg :: Typeable a => CExpression a -> CArg

data CStatement a where
    Return :: CExpression a -> CStatement a
    BindExpr :: Typeable a => CExpression a -> Int -> CStatement b -> CStatement b
    Seq :: CStatement a -> CStatement a -> CStatement a
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    DefFun :: (Typeable b)
                => TypeRep
                -> Int
                -> CParams -> CStatement b -> CStatement b
    DefVar :: Typeable a => Int -> CExpression a -> CStatement b
    UpdateVar :: Typeable a => Int -> CExpression a -> CStatement b
    While :: CExpression Bool -> CStatement a -> CStatement a
    Skip :: CStatement a

type LiftEnv = Map.Map Int CParams
type Lifted a = [CStatement a]

translate :: CL.CStatement a -> CStatement a
translate CL.Skip = Skip
translate (CL.Return x) = Return x
translate (CL.DefVar i x) = DefVar i x
translate (CL.UpdateVar i x) = UpdateVar i x
translate (CL.While cond x) = While cond (translate x)
translate (CL.Seq x y) = Seq (translate x) (translate y)
translate (CL.BindExpr x i s) = BindExpr x i (translate s)
translate (CL.If cond x y) = If cond (translate x) (translate y)
translate (CL.DefFun tret ifun (ip, tp) body) = DefFun (typeRep tret) ifun [CParam ip tp] (translate body)

paramsToMap :: CParams -> CParamMap
paramsToMap = Map.fromList . Prelude.map (\p@(CParam i _) -> (i, p))

merge :: (CParamMap, CParamMap) -> (CParamMap, CParamMap) -> (CParamMap, CParamMap)
merge (xfree, xbound) (yfree, ybound) = (Map.union xfree yfree, Map.union xbound ybound)

-- free, bound
freeVarsExpr :: forall a. CExpression a -> (CParamMap, CParamMap)
freeVarsExpr (Not x) = freeVarsExpr x
freeVarsExpr (Fst x) = freeVarsExpr x
freeVarsExpr (Snd x) = freeVarsExpr x
freeVarsExpr (IsEmpty l) = freeVarsExpr l
freeVarsExpr (TailList l) = freeVarsExpr l
freeVarsExpr (HeadList l) = freeVarsExpr l
freeVarsExpr (IndexList l _) = freeVarsExpr l
freeVarsExpr (Val _) = (Map.empty, Map.empty)
freeVarsExpr EmptyList = (Map.empty, Map.empty)
freeVarsExpr (Prod x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LIntOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (LCmpOp _ x y) = merge (freeVarsExpr x) (freeVarsExpr y)
freeVarsExpr (CallExpr f x) = merge (freeVarsExpr f) (freeVarsExpr x)
freeVarsExpr (ConsList l x) = merge (freeVarsExpr l) (freeVarsExpr x)
freeVarsExpr (Var i) = (Map.singleton i (CParam i (Proxy :: Proxy a)), Map.empty)
freeVarsExpr (Ternary cond thn els) = merge (merge (freeVarsExpr cond) (freeVarsExpr thn)) (freeVarsExpr els)

-- free, bound
freeVarsStmt :: Typeable a => CStatement a -> (CParamMap, CParamMap)
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

freeVars :: Typeable a => CStatement a -> CParamMap
freeVars s =
    let (free, bound) = freeVarsStmt s
    in Map.difference free bound

applyArgs :: forall a. Typeable a => CExpression a -> CParams -> CExpression a
applyArgs acc [] = acc
applyArgs acc ((CParam i (Proxy :: Proxy p)) : vs) =
    applyArgs (CallExpr (unsafeCoerce acc) (Var i :: CExpression p)) vs

rewriteExpr :: LiftEnv -> CExpression a -> CExpression a
rewriteExpr env (CallExpr (Var f) x) =
  let x' = rewriteExpr env x
      base = CallExpr (Var f) x'
  in case Map.lookup f env of
       Just extraVars -> applyArgs base extraVars
       Nothing -> base
rewriteExpr m  (Not x) = Not (rewriteExpr m x)
rewriteExpr m  (Fst x) = Fst (rewriteExpr m x)
rewriteExpr m  (Snd x) = Snd (rewriteExpr m x)
rewriteExpr m  (IsEmpty x) = IsEmpty (rewriteExpr m x)
rewriteExpr m  (HeadList x) = HeadList (rewriteExpr m x)
rewriteExpr m  (TailList x) = TailList (rewriteExpr m x)
rewriteExpr m  (IndexList l i) = IndexList (rewriteExpr m l) i
rewriteExpr m  (Prod x y) = Prod (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (ConsList l x) = ConsList (rewriteExpr m l) (rewriteExpr m x)
rewriteExpr m  (CallExpr f x) = CallExpr (rewriteExpr m f) (rewriteExpr m x)
rewriteExpr m  (LIntOp op x y) = LIntOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (LCmpOp op x y) = LCmpOp op (rewriteExpr m x) (rewriteExpr m y)
rewriteExpr m  (Ternary x y z) = Ternary (rewriteExpr m x) (rewriteExpr m y) (rewriteExpr m z)
rewriteExpr _ x = x

rewriteStmt :: LiftEnv -> CStatement a -> CStatement a
rewriteStmt m (BindExpr x i y) = BindExpr (rewriteExpr m x) i (rewriteStmt m y)
rewriteStmt m (Seq x y) = Seq (rewriteStmt m x) (rewriteStmt m y)
rewriteStmt m (If cond x y) = If (rewriteExpr m cond) (rewriteStmt m x) (rewriteStmt m y)
rewriteStmt m (While cond x) = While (rewriteExpr m cond) (rewriteStmt m x)
rewriteStmt m (DefFun tret ifun params body) = DefFun tret ifun params (rewriteStmt m body)
rewriteStmt m (UpdateVar i x) = UpdateVar i (rewriteExpr m x)
rewriteStmt m (DefVar i x) = DefVar i (rewriteExpr m x)
rewriteStmt m (Return x) = Return (rewriteExpr m x)
rewriteStmt _ Skip = Skip

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

-- for a function (int), given the amount of params, check that every call site has at least that many applications
checkCallExpr :: Int -> Int -> CExpression a -> Bool
checkCallExpr fun params expr =
    let (f, args) = collectArgs expr
    in case f of
        Var i | i == fun -> length args >= params
        _ -> True  -- not a call to our function, fine
  where
    collectArgs :: CExpression a -> (CExpression a, [CArg])
    collectArgs (CallExpr f args) =
        let (f', args') = collectArgs (unsafeCoerce f)
        in (f', args' ++ [CArg args])
    collectArgs e = (e, [])

checkCallStmt :: Int -> Int -> CStatement a -> Bool
checkCallStmt fun params stmt = case stmt of
    Return e       -> checkCallExpr fun params e
    Seq x y        -> checkCallStmt fun params x && checkCallStmt fun params y
    If c t e       -> checkCallExpr fun params c &&
                      checkCallStmt fun params t &&
                      checkCallStmt fun params e
    BindExpr e _ s -> checkCallExpr fun params e && checkCallStmt fun params s
    DefFun _ _ _ b -> checkCallStmt fun params b
    While c b      -> checkCallExpr fun params c && checkCallStmt fun params b
    _              -> True

-- retrun merged and map of functions to their new number of params
mergeLambdas :: CStatement a -> Map.Map Int Int -> (CStatement a, Map.Map Int Int)
mergeLambdas (DefFun tret ifun params body) m =
    case body of
        (Seq (DefFun tret1 ifun1 params1 body1) (Return (Var _))) ->
            let newParams = params ++ params1
                canMerge  = checkCallStmt ifun1 (length params1) (unsafeCoerce body1)
            in if canMerge then
                let newDef = unsafeCoerce $ DefFun tret1 ifun newParams (unsafeCoerce body1 :: CStatement Int)
                    newMap    = Map.insert ifun (length newParams) m
                in (unsafeCoerce newDef, newMap)
                else let (body', m') = mergeLambdas body m
                    in (DefFun tret ifun params body', m')
        _ -> let (body', m') = mergeLambdas body m
            in (DefFun tret ifun params body', m')
mergeLambdas (Seq x y) m =
    let (x', m')  = mergeLambdas x m
        (y', m'') = mergeLambdas y m'
    in (Seq x' y', m'')
mergeLambdas stmt m = (stmt, m)

-- SHOW

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
        ("[]",   [_])     -> "Node* " ++ s ++ "(" ++ showCParams params ++ ")"
        ("(,)",  [a, _]) -> showProx a ++ "* " ++ s ++ "(" ++ showCParams params ++ ")"
        ("->",   [a, b]) -> showProx b ++ " (*" ++ s ++ "(" ++ showCParams params ++ ")" ++ ")(" ++ showProx a ++ ")"
        _                -> show p ++ s ++ "(" ++ showCParams params ++ ")"

showCParams :: CParams -> String
showCParams [] = ""
showCParams [CParam i t] = showProxVar ("v" ++ show i) (typeRep t)
showCParams (i:is) = showCParams [i] ++ ", " ++ showCParams is

showCExpression :: CExpression a -> Map.Map Int Int -> String
showCExpression (Var i) _ = "v" ++ show i
showCExpression (Not x) m = "!" ++ showCExpression x m
showCExpression (LIntOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBinOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (LCmpOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showCmpOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (Val v) _ = CL.showCValue v
showCExpression (CallExpr f arg) m = -- merges together nested calls if I merged together the params earlier
    let (func, args) = collectArgs (CallExpr f arg)
    in case func of
        Var i -> case Map.lookup i m of
            Just n ->
                let (merged, rest) = Prelude.splitAt n args
                    baseCall = "v" ++ show i ++ "(" ++ intercalate ", " (Prelude.map (\(CArg a) -> showCExpression a m) merged) ++ ")"
                in if Prelude.null rest
                   then baseCall
                   else baseCall ++ "(" ++ intercalate ", " (Prelude.map (\(CArg a) -> showCExpression a m) rest) ++ ")"
            Nothing -> showCExpression func m ++ "(" ++ showCExpression arg m ++ ")"
        _ -> showCExpression func m ++ "(" ++ showCExpression arg m ++ ")"
  where
    collectArgs :: CExpression a -> (CExpression a, [CArg])
    collectArgs (CallExpr fun1 arg1) =
        let (func, args) = collectArgs (unsafeCoerce fun1)
        in (func, args ++ [CArg arg1])
    collectArgs e = (e, [])
showCExpression (Prod l r) m = "(" ++ showCExpression l m ++ "," ++ showCExpression r m ++ ")"
showCExpression (Fst p) m = showCExpression p m ++ "[0]"
showCExpression (Snd p) m = showCExpression p m ++ "[1]"
showCExpression EmptyList _ = "NULL"
showCExpression (ConsList x l) m = "cons(&(" ++ showProx (typeRep x) ++ "){" ++ showCExpression x m ++ "}, " ++ showCExpression l m ++ ")"
showCExpression (IsEmpty l) m = "isEmpty(" ++ showCExpression l m ++ ")"
showCExpression (HeadList l) m = "*(" ++ CL.showProxList (typeRep l) ++ ")" ++ "head(" ++ showCExpression l m ++ ")"
showCExpression (TailList l) m = "tail(" ++ showCExpression l m ++ ")"
showCExpression (IndexList l i) m = showCExpression l m ++ "[" ++ showCExpression i m ++ "]"
showCExpression (Ternary cond thn els) m = "(" ++ showCExpression cond m ++ ") ? (" ++ showCExpression thn m ++ ") : (" ++ showCExpression els m ++ ")"

showCStmt :: Int -> Map.Map Int Int -> CStatement a -> String
showCStmt indent m (UpdateVar i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m (If cond t f) =
    "\n" ++ indentStr indent ++ "if " ++ showCExpression cond m ++ " {"
    ++  showCStmt (indent + 1) m t
    ++ "\n" ++ indentStr indent ++ "} else {"
    ++ showCStmt (indent + 1) m f
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond m ++ " {"
    ++ showCStmt (indent + 1) m body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m (BindExpr x i y) =
    "\n" ++ indentStr indent ++ "let v" ++ show i ++ " = " ++ showCExpression x m ++ " in"
    ++ showCStmt (indent + 1) m y
showCStmt indent m (Seq x y) =
    showCStmt indent m x ++ showCStmt indent m y
showCStmt indent m (DefFun prox ifun params body) =
    "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params prox ++ " {"
    ++ showCStmt (indent + 1) m body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent m (DefVar i f) =  "\n" ++ indentStr indent ++ showProxVar ("v" ++ show i) (typeRep f) ++ " = " ++ showCExpression f m ++ ";"
showCStmt indent m (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x m ++ ";"
showCStmt _ _ Skip = ""

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
usesListExpr IsEmpty{} = True
usesListExpr ConsList{} = True
usesListExpr HeadList{} = True
usesListExpr TailList{} = True
usesListExpr IndexList{} = True
usesListExpr (Not x) = usesListExpr x
usesListExpr (Fst x) = usesListExpr x
usesListExpr (Snd x) = usesListExpr x
usesListExpr (Prod x y) = usesListExpr x || usesListExpr y
usesListExpr (CallExpr f x) = usesListExpr f || usesListExpr x
usesListExpr (LCmpOp _ x y) = usesListExpr x || usesListExpr y
usesListExpr (LIntOp _ x y) = usesListExpr x || usesListExpr y
usesListExpr (Ternary x y z) = usesListExpr x || usesListExpr y || usesListExpr z
usesListExpr (Val (CL.ListV _)) = True
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
makeFunDefs [DefFun tret ifun params _] = "\n" ++ showProxFunc ("v" ++ show ifun) params tret ++ ";"
makeFunDefs (i:is) = makeFunDefs[i] ++ makeFunDefs is

main :: IO ()
main = do
    let progName = "mapListCall"
    let (nl, c') = NL.translate 0 AL.mapListCall
        cl = evalState (CL.translate nl) c'
        c = translate cl

    putStrLn "--- Translating to CLang ---"
    putStrLn $ CL.showCStmt 0 cl

    putStrLn "\n--- Merging Lambdas ---"
    let (merged, mergedMap) = mergeLambdas c Map.empty
    putStrLn $ showCStmt 0 mergedMap merged 

    putStrLn "\n--- Printing C ---"
    let (cbody, defs) = lambdaLift merged
    let imports = "// imports" ++
                "\n#include <stdbool.h>" ++
                "\n#include <stdio.h>" ++
                "\n#include <stdlib.h>\n"
    let funDefs = makeFunDefs defs
    let ret = findFirstReturn cbody
    let bodyWithoutRet = removeFirstReturn cbody
    let body = showCStmt 0 mergedMap bodyWithoutRet ++ "\nint main(void) {\n" ++
                    "  printf(\"%d\\n\", " ++ showCExpression ret mergedMap ++ ");\n" ++
                    "  return 0;\n}\n"
    let contentHeader = if usesList cbody then imports ++ listPreamble else imports
    let content = contentHeader ++ "\n// Function Definitions" ++ funDefs ++ "\n\n// Compiled Program" ++ body

    -- writing to file
    let fileName = "outputs/" ++ progName ++ "_output.c"
    handle <- openFile fileName WriteMode
    hPutStrLn handle content
    hClose handle
    putStrLn $ "Successfully wrote to " ++ fileName