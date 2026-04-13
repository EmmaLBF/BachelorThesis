{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    -- Unit :: CValue ()

data CExpression a where
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a
    CallExpr :: (Typeable a, Typeable b) => CExpression (a -> b) -> CExpression a -> CExpression b
    Return :: CExpression a -> CExpression a
    Bind :: (Typeable a, Typeable b) => CExpression a -> Int -> CExpression b -> CExpression b -- give a name to outcome of a to use in 
    Seq :: CExpression a -> CExpression b -> CExpression b -- Do two things in succesion, but do not give intermediate outcome a name, specialization of bind

    DefFun :: (Typeable a) => Int -> CExpression b -> CExpression (a -> b) -- define a function of 1 parameter, use the Int as the name of the parameter
    Not :: CExpression Bool -> CExpression Bool

    DefVar :: (Typeable a) => CExpression a -> CExpression a -- Define a variable
    UpdateVar :: (Typeable a) => Int -> CExpression a -> CExpression ()  -- Update variable 
    If :: CExpression Bool -> CExpression a -> CExpression a -> CExpression a
    While :: CExpression Bool -> CExpression a -> CExpression () -- condition + body
    Prod :: CExpression a -> CExpression b -> CExpression (a, b)  -- | Make a tuple
    Fst :: CExpression (a, b) -> CExpression a -- | Project left
    Snd :: CExpression (a, b) -> CExpression b -- | Project right

translate :: forall a. Int -> NL.NamedLang a -> (CExpression a, Int)
translate c (NL.Var x) = (Var x, c)
translate c (NL.LInt i) = (Val (IntV i), c)
translate c (NL.LBool b) = (Val (BoolV b), c)
translate c (NL.LIntOp op x y) = let (x', c') = translate c x
                                     (y', c'') = translate c' y
                                in (LIntOp op x' y', c'')
translate c (NL.LCmpOp op x y) = let (x', c') = translate c x
                                     (y', c'') = translate c' y
                                  in (LCmpOp op x' y', c'')
translate c (NL.If cond thn els) = let  (cond', c') = translate c cond
                                        (thn', c'') = translate c' thn
                                        (els', c''') = translate c'' els
                                    in (If cond' thn' els', c''')
translate c (NL.Apply f x) = let (f', c') = translate c f
                                 (x', c'') = translate c' x
                               in (CallExpr f' x', c'')
translate c (NL.Lam i body) = let (body', c') = translate i body
                                in (DefFun i body', c')
translate c (NL.Prod a b) = let (a', c') = translate c a
                                (b', c'') = translate c' b
                               in (Prod a' b', c'')
translate c (NL.Fst p) = let (p', c') = translate c p
                            in (Fst p', c')
translate c (NL.Snd p) = let (p', c') = translate c p
                            in (Snd p', c')
translate c (NL.Fix (NL.Lam i1 (NL.Lam i2 (NL.If cond thn els)))) =
    let (thn', c') = translate c thn
        (cond', c'') = translate c' cond
        (els', c''') = translate c'' els
        body''      = rewriteRecCall i1 i2 els'
        v = Var i1 :: CExpression (a -> a)
    in (DefFun i2 (Bind (DefVar v) c' (Seq (Seq (UpdateVar i1 thn') (While (Not cond') body'')) (Return (Var i1)))), (c' + 1))
-- translate c (NL.Fix f) =
--     let (body', c')  = translate c f
--     in case body' of
--         (DefFun i body) -> (CallExpr body' (Var i), c')
--         _ -> (CallExpr body' (Var c), c' + 1)
-- translate c (NL.Fix (NL.Lam i body)) =
--     let (body', c') = translate i body
--         funDef = DefFun i body'
--     in (Bind funDef c' (CallExpr (Var c') (Var c')), c' + 1)

translate c (NL.Fix f) =
    let (body', c')  = translate c f
    in (CallExpr body' (Var c), c' + 1)


rewriteRecCall :: Int -> Int -> CExpression a -> CExpression a
rewriteRecCall f n (CallExpr (Var i) newArg)
    | i == f = Seq (UpdateVar n (rewriteRecCall f n newArg)) (Var n)
rewriteRecCall f n (LIntOp op (CallExpr (Var il) newArgl) (CallExpr (Var ir) newArgr))
    | il == f && ir == f = let rewriteLeft = rewriteRecCall f n newArgl
                               rewriteRight = rewriteRecCall f n newArgr
                               lhs = Seq (UpdateVar f (LIntOp op (Var f) (Var n))) (UpdateVar n rewriteLeft)
                               rhs = Seq (Seq (UpdateVar f (LIntOp op (Var f) (Var n))) (UpdateVar n rewriteRight)) (Var f)
                            in Seq lhs rhs
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
eval (Not x) m = let (x', m') = eval x m
                in (not x', m')
eval _ m = error "unkown expr"

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

showCExpression :: Int -> CExpression a -> Map Int Dynamic -> String
showCExpression _ (Var i) _ = "v" ++ show i
showCExpression indent (Not x) m = "!" ++ showCExpression indent x m
showCExpression indent (LIntOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showBinOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression indent (LCmpOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showCmpOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression _ (Val v) _ = showCValue v
showCExpression indent (CallExpr f arg) m = showCExpression indent f m ++ "(" ++ showCExpression indent arg m ++ ")"
showCExpression indent (Return x) m = "[Return " ++ showCExpression indent x m ++ "]"
showCExpression indent (DefVar f) m = "def " ++ showCExpression indent f m
showCExpression indent (Prod l r) m = "(" ++ showCExpression indent l m ++ "," ++ showCExpression indent r m ++ ")"
showCExpression indent (Fst p) m = showCExpression indent p m ++ "[0]"
showCExpression indent (Snd p) m = showCExpression indent p m ++ "[1]"
showCExpression indent (UpdateVar i x) m = "v" ++ show i ++ " =~ " ++ showCExpression indent x m

showCExpression indent (If cond t f) m = 
    "\n" ++ indentStr indent ++ "if " ++ showCExpression indent cond m ++ " {\n"
  ++ indentStr (indent + 1) ++ showCExpression (indent + 1) t m ++ "\n"
  ++ indentStr indent ++ "} else {\n"
  ++ indentStr (indent + 1) ++ showCExpression (indent + 1) f m ++ "\n"
  ++ indentStr indent ++ "}"
showCExpression indent (While cond body) m =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression indent cond m ++ " {\n"
  ++ indentStr (indent + 1) ++ showCExpression (indent + 1) body m ++ "\n"
  ++ indentStr indent ++ "}"
showCExpression indent (Bind x i y) m = showCExpression indent x m ++ "\n" ++ indentStr indent ++ showCExpression indent y m
showCExpression indent (Seq x y) m = showCExpression indent x m ++ "\n" ++ indentStr indent ++ showCExpression indent y m
showCExpression indent (DefFun i f) m = 
    indentStr indent ++ "function (v" ++ show i ++ ") {\n"
  ++ indentStr (indent + 1) ++ showCExpression (indent + 1) f m ++ "\n"
  ++ indentStr indent ++ "}"


main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.gcdLang
        (cl, _) = translate c' nl
        -- (ev, c'') = eval cl Map.empty
    print c'
    putStrLn "--- Translating CL ---"
    putStrLn $ showCExpression 0 cl Map.empty
    -- putStrLn $ show (ev 5)
