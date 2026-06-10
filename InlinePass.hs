{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InlinePass where
import C
import CDefs
import Utils
import AST
import qualified Data.Map as Map
import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe

-- Single inlining pass, finds list of functions that are safe to inline (called exactly once)
-- It then tries to inline all of these functions
-- It returns a list of the functions that were removed so I can get rid of them later
inlinePass :: [CStatement a] -> CStatement a -> (CStatement a, [Int])
inlinePass defs body =
    let globalInfo = getGlobalInfo body emptyGlobalInfo
        safeToInline = Map.keys $ Map.filter (== 1) $ Map.filterWithKey
            (\i _ -> case findFunDef i defs of
                Just DefFun {} -> True
                _ -> False) (functionCallsGlobal globalInfo)
    in foldr (\i (b, removed) ->
            let b' = inlineOne i defs b
            in if b /= b' then (b', i : removed) else (b, removed)
        ) (body, []) safeToInline

-- replace return statement inside both branches of an if statement
-- so that we can then inline that if statement
-- using id of inlined function as fresh var to hold result
replaceReturn :: Int -> CType -> CStatement a -> CStatement a
replaceReturn i t (Return x) = UpdateVar t i x
replaceReturn i t (Seq x y) = Seq x (replaceReturn i t y)
replaceReturn i t (BindExpr t' x j y) = BindExpr t' x j (replaceReturn i t y)
replaceReturn i t (If c x y) = If c (replaceReturn i t x) (replaceReturn i t y)
replaceReturn i t (While c x) = While c (replaceReturn i t x)
replaceReturn _ _ x = x

-- Inline all calls to function i throughout body
-- if the function body ends in if we need to handle two returns
    -- replace the returns with an accumulator var
inlineOne :: Int -> [CStatement a] -> CStatement a -> CStatement a
inlineOne i defs body =
    case findFunDef i defs of
        Just (DefFun tret ifun params fbody) ->
            if endsInIf fbody then (
                let retExpr = Var tret ifun
                    bodyNoRet = Seq (DefVar tret ifun (Val (defaultVal tret))) (replaceReturn ifun tret fbody)
                in inlineCallsTo i params bodyNoRet retExpr body)
            else (
                let retExpr = fromMaybe (error "no return") (findReturn fbody)
                    bodyNoRet = removeFirstReturn fbody
                in inlineCallsTo i params bodyNoRet retExpr body)
        _ -> body

-- takes a pair of cparam and arg and turns them into a var definition
-- so that the inlined body can still use its arguments
inlineArgs :: (CParam, CArg) -> CStatement a -> CStatement a
inlineArgs (param, CArg _ arg) acc =
    case (param, arg) of
        (CParam ip tp, _) -> Seq (DefVar tp ip arg) acc
        (CParamEnv ip, Val (EnvV ip'))
            | ip' == ip -> acc -- do not redefine env vars which are already defined, we don't want Env66* env66 = env66;
            | otherwise -> Seq (DefVar (CTPtr CTVoid) ip arg) acc
        (x, y) -> error ("mismatch arg and param = " ++ show x ++ " | " ++ showCExpression y Map.empty)

-- Replace all CallExpr (Var i) args with inlined body
-- Replace ternary with if so that we can add the pre work
inlineCallsTo :: Int -> CParams -> CStatement a -> CExpression a -> CStatement a -> CStatement a
inlineCallsTo i params fbodyNoRet retExpr = goStmt
  where
    goStmt (Seq x y) = Seq (goStmt x) (goStmt y)
    goStmt (DefFun t j ps b) = DefFun t j ps (goStmt b)
    goStmt (Return x) =
        let (pre, x') = goExpr x
        in Seq pre (Return x')
    goStmt (BindExpr t x j y) =
        let (pre, x') = goExpr x
        in Seq pre (BindExpr t x' j (goStmt y))
    goStmt (If c x y) =
        let (pre, c') = goExpr c
        in Seq pre (If c' (goStmt x) (goStmt y))
    goStmt (While c x) =
        let (pre, c') = goExpr c
        in Seq pre (While c' (goStmt x))
    goStmt (DefVar t j x) =
        let (pre, x') = goExpr x
        in Seq pre (DefVar t j x')
    goStmt (UpdateVar t j x) =
        let (pre, x') = goExpr x
        in Seq pre (UpdateVar t j x')
    goStmt x = x

    goExpr :: CExpression b -> (CStatement a, CExpression b)
    goExpr expr@(CallExpr tf tx f x) =
        let (func, args) = collectArgs expr
        in case func of
            Var _ j | j == i -> -- called function is equal to the one we're inlining
                let funArgs = take (length params) args
                    bindings = foldr inlineArgs Skip (zip params funArgs)
                    pre = Seq bindings fbodyNoRet
                in (unsafeCoerce pre, unsafeCoerce retExpr)
            _ ->
                let (px, x') = goExpr x
                    (pf, f') = goExpr f
                in (Seq pf px, CallExpr tf tx f' x')
    goExpr (Not x) = let (p, x') = goExpr x in (p, Not x')
    goExpr (Abs x) = let (p, x') = goExpr x in (p, Abs x')
    goExpr (IsEmpty t x) = let (p, x') = goExpr x in (p, IsEmpty t x')
    goExpr (HeadList t x) = let (p, x') = goExpr x in (p, HeadList t x')
    goExpr (TailList t x) = let (p, x') = goExpr x in (p, TailList t x')
    goExpr (Fst tp tr x) = let (p, x') = goExpr x in (p, Fst tp tr x')
    goExpr (Snd tp tr x) = let (p, x') = goExpr x in (p, Snd tp tr x')
    goExpr (CastExpr t x)= let (p, x') = goExpr x in (p, CastExpr t x')
    goExpr (Unbox t x) = let (p, x') = goExpr x in (p, Unbox t x')
    goExpr (Box t x) = let (p, x') = goExpr x in (p, Box t x')
    goExpr (Ternary tp c t e) =
        let (pc, c') = goExpr c
            (pt, t') = goExpr t
            (pe, e') = goExpr e
        in (Seq pc (Seq pt pe), Ternary tp c' t' e')
    goExpr (ConsList t x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, ConsList t x' y')
    goExpr (LIntOp op x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, LIntOp op x' y')
    goExpr (LCmpOp op x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, LCmpOp op x' y')
    goExpr (LBoolOp op x y) =
        let (px, x') = goExpr x
            (py, y') = goExpr y
        in (Seq px py, LBoolOp op x' y')
    goExpr (ApplyClosure tx f x) =
        let (pf, f') = goExpr f
            (px, x') = goExpr x
        in (Seq pf px, ApplyClosure tx f' x')
    goExpr x = (Skip, x)

-- Keep inlining until nothing changes, bool indicates if anything was removed
inlineUntilFixed :: CStatement a -> CStatement a
inlineUntilFixed body =
    let (body', removed) = inlinePass (getDefs body) body
    in if null removed
       then body'
       else inlineUntilFixed (removeDeadFuns removed body')

-- pass list of called funs
removeDeadFuns :: [Int] -> CStatement a -> CStatement a
removeDeadFuns removedFuns def@(DefFun _ ifun _ _) =
    if ifun `elem` removedFuns then Skip else def -- still used
removeDeadFuns m (Seq x y) = Seq (removeDeadFuns m x) (removeDeadFuns m y)
removeDeadFuns _ x = x
