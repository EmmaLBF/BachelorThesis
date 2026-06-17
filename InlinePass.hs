{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module InlinePass where
import C
import CDefs
import Utils
import AST
import qualified Data.Map as Map

-- inlining pass, finds list of functions that are safe to inline (called exactly once)
-- It then tries to inline all of these functions
-- It returns a list of the functions that were removed so I can get rid of them later
inlinePass :: CStatement -> CStatement
inlinePass body =
    let globalInfo = getGlobalInfo body emptyGlobalInfo
        safeToInline = Map.keys $ Map.filter (== 1) (functionCallsGlobal globalInfo)
        (body', removed') = 
            foldr (\i (b, removed) ->
                let b' = inlineOne i b
                in if b /= b' then (b', i : removed) else (b, removed)
            ) (body, []) safeToInline
    in removeDeadFuns removed' body'

-- replace return statement inside both branches of an if statement
-- so that we can then inline that if statement
-- using id of inlined function as fresh var to hold result
replaceReturn :: Int -> CType -> CStatement -> CStatement
replaceReturn i t (Return x) = UpdateVar t i x
replaceReturn i t s = mapChildrenStmt (replaceReturn i t) id s

-- Inline all calls to function i throughout body
-- if the function body ends in if we need to handle two returns
    -- replace the returns with an accumulator var, can have same id as original function
    -- since functions are only inlined if they are called once, so no chance of duplicate var
inlineOne :: Int -> CStatement -> CStatement
inlineOne i body =
    let defs = getDefs body
    in case findFunDef i defs of
        Just (DefFun tret ifun params fbody) ->
            let (retExpr, bodyNoRet) = 
                    if endsInIf fbody 
                    then (let accVar = DefVar tret ifun (Val (defaultVal tret))
                          in (Var tret ifun, Seq accVar (replaceReturn ifun tret fbody)))
                    else (findReturn fbody, removeFirstReturn fbody)
            in inlineCallsTo i params retExpr bodyNoRet body
        _ -> body

-- takes a pair of cparam and arg and turns them into a var definition
-- so that the inlined body can still use its arguments
inlineArgs :: (CParam, CArg) -> CStatement -> CStatement
inlineArgs (param, CArg _ arg) acc =
    case (param, arg) of
        (CParam ip tp, _) -> Seq (DefVar tp ip arg) acc
        (CParamEnv ip, Val (EnvV ip'))
            | ip' == ip -> acc -- do not redefine env vars which are already defined, we don't want Env66* env66 = env66;
            | otherwise -> Seq (DefVar (CTPtr CTVoid) ip arg) acc
        (x, y) -> error ("mismatch arg and param = " ++ show x ++ " | " ++ showCExpression y Map.empty)

-- Replace all CallExpr (Var i) args with inlined body
-- Replace ternary with if so that we can add the pre work
inlineCallsTo :: Int -> CParams -> CExpression -> CStatement -> CStatement -> CStatement
inlineCallsTo i params retExpr fbody s = 
    let s' = mapChildrenStmt (inlineCallsTo i params retExpr fbody) (snd . goExpr) s
        pres = map (fst . goExpr) (childExprsStmt s)
        pre = foldr Seq Skip pres
    in if null pres then s' else Seq pre s'

    where
    goExpr :: CExpression -> (CStatement, CExpression)
    goExpr e@CallExpr{} =
        let (func, args) = collectArgs e
        in case func of
            Var _ j | j == i -> -- called function is equal to the one we're inlining
                let funArgs = take (length params) args
                    bindings = foldr inlineArgs Skip (zip params funArgs)
                in (Seq bindings fbody, retExpr)
            _ ->
                let pre = foldr ((Seq . fst) . goExpr) Skip (childrenExpr e)
                in (pre, mapChildrenExpr (snd . goExpr) e)
    goExpr e =
        let pre = foldr ((Seq . fst) . goExpr) Skip (childrenExpr e)
        in (pre, mapChildrenExpr (snd . goExpr) e)

-- pass list of called funs
removeDeadFuns :: [Int] -> CStatement -> CStatement
removeDeadFuns removedFuns def@(DefFun _ ifun _ _) =
    if ifun `elem` removedFuns then Skip else def -- still used
removeDeadFuns m (Seq x y) = Seq (removeDeadFuns m x) (removeDeadFuns m y)
removeDeadFuns _ x = x
