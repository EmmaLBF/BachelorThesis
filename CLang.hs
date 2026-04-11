{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Fix

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    -- FuncV :: (CValue a -> CValue b) -> CValue (a -> b)

data CExpression a where
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Not :: CExpression Bool -> CExpression Bool
    Val :: CValue a -> CExpression a
    CallExpr :: (Typeable a, Typeable b) => CExpression (a -> b) -> CExpression a -> CExpression b
    Return :: (Typeable a) => CExpression a -> CExpression a
    Bind :: (Typeable a, Typeable b) => CExpression a -> Int -> CExpression b -> CExpression b -- give a name to outcome of a to use in 
    Seq :: (Typeable a, Typeable b) => CExpression a -> CExpression b -> CExpression b -- Do two things in succesion, but do not give intermediate outcome a name, specialization of bind

    DefFun :: (Typeable a, Typeable b) => Int -> CExpression b -> CExpression (a -> b) -- define a function of 1 parameter, use the Int as the name of the parameter
    -- FixExpr :: (Typeable a) => CExpression (a -> a) -> CExpression a

    DefVar :: (Typeable a) => CExpression a -> CExpression a -- Define a variable
    UpdateVar :: (Typeable a) => Int -> CExpression a -> CExpression ()  -- Update variable 
    If :: (Typeable a) => CExpression Bool -> CExpression a -> CExpression a -> CExpression a
    While :: (Typeable a) => CExpression Bool -> CExpression a -> CExpression a -- condition + body
    Prod :: (Typeable a, Typeable b) => CExpression a -> CExpression b -> CExpression (a, b)  -- | Make a tuple
    Fst :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression a -- | Project left
    Snd :: (Typeable a, Typeable b) => CExpression (a, b) -> CExpression b -- | Project right

translate :: forall a. Map Int Dynamic -> Int -> NL.NamedLang a -> (CExpression a, Int)
translate m c (NL.Var x) = (Var x, c)
translate m c (NL.LInt i) = (Val (IntV i), c)
translate m c (NL.LBool b) = (Val (BoolV b), c)
translate m c (NL.LIntOp op x y) = let (x', c') = translate m c x
                                       (y', c'') = translate m c' y
                                in (LIntOp op x' y', c'')
translate m c (NL.LCmpOp op x y) = let (x', c') = translate m c x
                                       (y', c'') = translate m c' y
                                  in (LCmpOp op x' y', c'')
translate m c (NL.If cond thn els) = let (cond', c') = translate m c cond
                                         (thn', c'') = translate m c' thn
                                         (els', c''') = translate m c'' els
                                    in (If cond' thn' els', c''')
translate m c (NL.Apply f x) = let (f', c') = translate m c f
                                   (x', c'') = translate m c' x
                               in (CallExpr f' x', c'')
translate m c (NL.Lam i body) = let (body', c') = translate m i body
                                in (DefFun i body', c')
translate m c (NL.Prod a b) = let (a', c') = translate m c a
                                  (b', c'') = translate m c' b
                               in (Prod a' b', c'')
translate m c (NL.Fst p) = let (p', c') = translate m c p
                            in (Fst p', c')
translate m c (NL.Snd p) = let (p', c') = translate m c p
                            in (Snd p', c')
translate m c (NL.Fix (NL.Lam i1 (NL.Lam i2 (NL.If cond thn els)))) =
    let (thn', c') = translate m c thn
        (cond', c'') = translate m c' cond
        (els', c''') = translate m c'' els
        body''      = rewriteRecCall i1 i2 els'
        v = Var i1 :: CExpression (a -> a)
    in (DefFun i2 (Bind (DefVar v) c' (Seq (UpdateVar i1 thn') (While (Not cond') body''))), c')
-- translate m c (NL.Fix (NL.Lam i1 (NL.Lam i2 body))) =
--     let (body', c') = translate m i2 body
--         body''      = rewriteRecCall i1 i2 body'
--         v = Var i2 :: CExpression (a -> a)
--     in (DefFun i2 (Bind (DefVar v) c' (While (Val (BoolV True)) body'')), c')
translate m c (NL.Fix f) =
    let (body', c')  = translate m c f
    in (CallExpr body' (Var c), c' + 1)
-- translate m c (NL.Fix (NL.Lam x0 (NL.Lam x1 (body :: NL.NamedLang b1)))) =
--     let (body', c')  = translate m x1 body
--         body''       = rewriteRecCall body'
--         varInit      :: CExpression a2
--         varInit      = DefVar (Var x1)
--     in (DefFun x1 (Bind varInit c' (While (Val (BoolV True)) body'')), c' + 1)
-- translate m c (NL.Fix f) = let (f', c') = translate m c f
--                             in (While (Val (BoolV True)) f', c')
    -- let (f', c') = translate m c f
    --     body'' = case f' of

    -- in (Bind (DefVar (Var i)) i (While (Val (BoolV True)) body''), c')

    -- let (body', c') = translate m i body
    --                                          funDef = DefRecFun c' i body'
    --                                     in (Bind funDef c' (CallExpr (Var c') (Var c')), c' + 1)

    -- let (cond', c') = translate m c cond
    --     (thn', c'') = translate m c' thn
    --     (els', c''') = translate m c' els
    -- in Seq (Bind (DefVar (cond')) c') (Seq (While (Not cond') ))


    -- let (body', c') = translate m i body
    --                                          funDef = DefFun i body'
    --                                     in (Bind funDef c' (CallExpr (Var c') (Var c')), (c' + 1))


rewriteRecCall :: Int -> Int -> CExpression a -> CExpression a
rewriteRecCall f n (CallExpr (Var i) newArg)
    | i == f = Seq (UpdateVar n (rewriteRecCall f n newArg)) (Var n)
rewriteRecCall f n (LIntOp op (CallExpr (Var i) newArg) y)
    | i == f = Seq (UpdateVar f (LIntOp op (Var f) (rewriteRecCall f n y))) (Seq (UpdateVar n (rewriteRecCall f n newArg)) (Var n))
rewriteRecCall f n (LIntOp op x (CallExpr (Var i) newArg))
    | i == f = Seq (UpdateVar f (LIntOp op (Var f) (rewriteRecCall f n x))) (Seq (UpdateVar n (rewriteRecCall f n newArg)) (Var n))
rewriteRecCall self param (LIntOp op x y) = LIntOp op (rewriteRecCall self param x) (rewriteRecCall self param y)
rewriteRecCall self param (LCmpOp op x y) = LCmpOp op (rewriteRecCall self param x) (rewriteRecCall self param y)
rewriteRecCall self param (If cond thn els) =
    If (rewriteRecCall self param cond)
       (rewriteRecCall self param thn)
       (rewriteRecCall self param els)
rewriteRecCall self param (Bind e n body) = Bind e n (rewriteRecCall self param body)
rewriteRecCall self param x = x
-- rewriteRecCall i (CallExpr (Var j) arg) | i == j = UpdateVar i arg
-- rewriteRecCall i (If cond thn els) = If cond (rewriteRecCall i thn) (rewriteRecCall i els)
-- rewriteRecCall i (Bind e n body) = Bind e n (rewriteRecCall i body)
-- rewriteRecCall _ e = e

valueToLiteral :: CValue a -> a
valueToLiteral (IntV i) = i
valueToLiteral (BoolV i) = i

eval :: CExpression a -> Map Int Dynamic -> (a, Map Int Dynamic)
eval (Val x) m = case x of
        (IntV i) -> (i, m)
        (BoolV i) -> (i, m)
eval (Return x) m = eval x m
eval (Var i) m = case Map.lookup i m of
                        Just dyn -> case fromDynamic dyn of
                            Just v -> (v,  m)
                            Nothing -> error "Type mismatch in env"
                        Nothing -> error "Variable not found"
eval (UpdateVar i x) m = let m' = Map.insert i (toDyn x) m
                         in ((), m')
eval (LIntOp op lhs rhs) m = let (lhs', m') = eval lhs m
                                 (rhs', m'') = eval rhs m'
                            in ( AL.binop op lhs' rhs', m'')
eval (LCmpOp op lhs rhs) m = let (lhs', m') = eval lhs m
                                 (rhs', m'') = eval rhs m'
                            in (AL.cmpop op lhs' rhs', m'')
eval (CallExpr f arg) m = let (fn, m') = eval f m
                              (arg', m'') = eval arg m'
                              res = fn arg'
                            in (res, m'')
eval (Seq x y) m = let (_, m') = eval x m
                   in eval y m'
eval (Bind x i y) m = let (p, m') = eval x m
                          m'' = Map.insert i (toDyn p) m'
                        in eval y m''
eval (If cond body alt) m = let (cond', m') = eval cond m
                            in if cond'
                               then eval body m'
                               else eval alt m'
eval (While cond body) m = let (cond', m') = eval cond m
                           in if cond'
                              then let (_, m'') = eval body m'
                                   in eval (While cond body) m''
                              else error "While loop has no return value"
eval (DefVar x) m = eval x m
eval (DefFun i body) m = (\x -> let m' = Map.insert i (toDyn x) m
                                  in fst (eval body m'), m)
eval (Prod l r) m = let (l', m') = eval l m
                        (r', m'') = eval r m'
                    in ((l',r'), m'')
eval (Fst p) m = let (p', m') = eval p m
                    in (fst p', m')
eval (Snd p) m = let (p', m') = eval p m
                    in (snd p', m')
-- evalExpr :: CExpression a -> Map Int Dynamic -> (CValue a, Map Int Dynamic)
-- evalExpr (Val x) m = (x, m)
-- evalExpr (Var i) m = case Map.lookup i m of
--                         Just dyn -> case fromDynamic dyn of
--                             Just v -> (v,  m)
--                             Nothing -> error "Type mismatch in env"
--                         Nothing -> error "Variable not found"
-- evalExpr (LIntOp op lhs rhs) m = let (lhs', m') = evalExpr lhs m
--                                      (rhs', m'') = evalExpr rhs m'
--                                     in (IntV (AL.binop op (valueToLiteral lhs') (valueToLiteral rhs')), m'')
-- evalExpr (LCmpOp op lhs rhs) m = let (lhs', m') = evalExpr lhs m
--                                      (rhs', m'') = evalExpr rhs m'
--                                     in (BoolV (AL.cmpop op (valueToLiteral lhs') (valueToLiteral rhs')), m'')
-- evalExpr (CallExpr f arg) m = let (fn, m') = evalFunc f m
--                                   (arg', _) = evalExpr arg m'
--                                   (res, m'') = fn arg'
--                               in (res, m'')

-- eval :: CExpression a -> Map Int Dynamic -> (CValue a, Map Int Dynamic)
-- eval (Return x) m = evalExpr x m
-- eval (Seq first f) m = let (p, m') = eval first m
--                         in eval (f p) m'
-- eval (If cond body alt) m = let (cond', m') = evalExpr cond m
--                             in if valueToLiteral cond'
--                                then eval body m'
--                                else eval alt m'
-- eval (While cond body) m = let (cond', m') = evalExpr cond m
--                            in if valueToLiteral cond' 
--                               then let (_, m'') = eval body m'
--                                    in eval (While cond body) m''
--                               else error "While loop has no return value"
-- eval (Fix f) m = (fix (\x -> fst (eval (case f x of
--                                         (Left e)  -> error e
--                                         (Right v) -> v) m)), m)
-- eval (Call f arg) m = let (stmt, m') = evalFunc f m
--                           (arg', _) = evalExpr arg m'
--                           (v, m'') = stmt arg'
--                       in (v, m'')
-- eval (Def _ _) _ = error "Cannot eval Def as a value — use evalFunc"

-- evalFunc :: CExpression (a -> b) -> Map Int Dynamic -> (CValue a -> (CValue b, Map Int Dynamic), Map Int Dynamic)
-- evalFunc (Def i body) m = (\x -> let m' = Map.insert i (toDyn x) m
--                                   in case body x of
--                                       Left e  -> error e
--                                       Right v -> eval v m', m)
-- evalFunc (If cond t f) m = let (cond', m') = evalExpr cond m
--                             in if valueToLiteral cond'
--                                then evalFunc t m'
--                                else evalFunc f m'
-- evalFunc (Call f arg) m = let (fn, m') = evalFunc f m
--                               (arg', _) = evalExpr arg m'
--                               (stmt, m'') = fn arg'
--                            in evalFunc stmt m''
-- evalFunc _ _ = error "Expected function-typed statement"

showBinOp :: AL.BinOp -> String
showBinOp Plus  = "+"
showBinOp Min   = "-"
showBinOp Times = "*"
showBinOp Div   = "/"
showBinOp Mod   = "%"

showCmpOp :: AL.CmpOp -> String
showCmpOp Eq = "=="
showCmpOp Lt = "<"
showCmpOp Gt = ">"

showCValue :: CValue a -> String
showCValue (IntV n)  = show n
showCValue (BoolV b) = show b
-- showCValue (FuncV _) = "<func>"

showCExpression :: Int -> CExpression a -> Map Int Dynamic -> String
showCExpression _ (Var i) _ = "v" ++ show i
showCExpression indent (Not x) m = "!" ++ showCExpression indent x m
showCExpression indent (LIntOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showBinOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression indent (LCmpOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showCmpOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression _ (Val v) _ = showCValue v
showCExpression indent (CallExpr f arg) m = indentStr indent ++ showCExpression indent f m ++ "(" ++ showCExpression indent arg m ++ ")"
showCExpression indent (Return x) m = indentStr indent ++ "[Return " ++ showCExpression indent x m ++ "]"
showCExpression indent (If cond t f) m = "\n" ++ indentStr indent ++ "if " ++ showCExpression 0 cond m ++ "\n" ++ indentStr indent ++ "then " ++ showCExpression 0 t m ++ "\n" ++ indentStr indent ++ "else " ++ showCExpression 0 f m
showCExpression indent (While cond body) m = "\n" ++ indentStr indent ++ "while " ++ showCExpression indent cond m ++ " {\n" ++ indentStr (indent + 1) ++ showCExpression (indent + 1) body m ++ "\n" ++ indentStr indent ++ "}"
showCExpression indent (Bind x i y) m = indentStr indent ++ showCExpression indent x m ++ showCExpression indent y m
showCExpression indent (Seq x y) m = indentStr indent ++ showCExpression indent x m ++ "\n" ++ indentStr indent ++ showCExpression indent y m
showCExpression indent (DefFun i f) m = "\n" ++ indentStr indent ++ "function (v" ++ show i ++ ") {\n" ++ showCExpression (indent + 1) f m ++ "\n" ++ indentStr indent ++ "}"
showCExpression indent (DefVar f) m   = "\n" ++ indentStr indent ++ "def " ++ showCExpression (indent + 1) f m
showCExpression indent (UpdateVar i x) m   = "\n" ++ indentStr indent ++ "v" ++ show i ++ " =~ " ++ showCExpression indent x m
showCExpression indent (Prod l r) m = "(" ++ showCExpression indent l m ++ "," ++ showCExpression indent r m ++ ")"
showCExpression indent (Fst p) m = showCExpression indent p m ++ "[0]"
showCExpression indent (Snd p) m = showCExpression indent p m ++ "[1]"


main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.fib
        (cl, _) = translate Map.empty c' nl
    putStrLn "--- Translating CL ---"
    putStrLn $ showCExpression 0 cl Map.empty
