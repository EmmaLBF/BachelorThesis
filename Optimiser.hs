{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Optimiser where
import C
import Debug.Trace
import qualified Data.Map as Map

-- remove boxing when not necessary


-- Inline function


-- Dead Code Elimination


-- int fact_iter(int n, int acc) {
--     while (1) {
--         if (n <= 1) return acc;
--         acc = acc * n;
--         n = n - 1;
--         // loop instead of recursive call
--     }
-- }

findReturn :: CStatement a -> Maybe (CExpression a)
findReturn (Return x) = Just x
findReturn (BindExpr _ _ _ y) = findReturn y
findReturn (Seq _ y) = findReturn y
findReturn (If _ x _) = findReturn x
findReturn (While _ x) = findReturn x
findReturn _ = Nothing

isTailRecursive :: Int -> CStatement a -> Bool
isTailRecursive ifun body =
    case findReturn body of
        Just (Ternary _ t e) -> hasTailCall ifun t || hasTailCall ifun e
        Just expr -> hasTailCall ifun expr
        Nothing   -> False

hasTailCall :: Int -> CExpression a -> Bool
hasTailCall ifun (CallExpr f _) = outermostVar f == Just ifun
hasTailCall ifun (Ternary _ t e) = hasTailCall ifun t || hasTailCall ifun e
hasTailCall _ _ = False

-- Function unrolling
unrollFunctions :: CStatement a -> CStatement a
unrollFunctions (DefFun tret ifun params body) = 
    if isTailRecursive ifun body then
        trace ("UNROLLING " ++ show ifun ++ " | " ++ showCStmt 0 Map.empty Map.empty Map.empty body) $
        DefFun tret ifun params (unrollFunctions body)
    else
        DefFun tret ifun params (unrollFunctions body)
unrollFunctions (Seq x y) = Seq (unrollFunctions x) (unrollFunctions y)
unrollFunctions s = s


-- Common Subexpression elimination




