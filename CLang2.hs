{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang2 where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import Control.Monad.State

indentStr :: Int -> String
indentStr n = replicate (2 * n) ' '

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()

data CExpression a where
    Var :: (Typeable a) => Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a
    Not :: CExpression Bool -> CExpression Bool
    CallExpr :: CExpression (a -> b) -> CExpression a -> CExpression b
    Prod :: CExpression a -> CExpression b -> CExpression (a, b)  -- | Make a tuple
    Fst :: CExpression (a, b) -> CExpression a -- | Project left
    Snd :: CExpression (a, b) -> CExpression b -- | Project right

-- in theory statements shouldn't have a type? since they don't return anything?
data CStatement a where
    Return :: CExpression a -> CStatement a
    Bind :: CStatement a -> Int -> CStatement b -> CStatement b
    Seq :: CStatement a -> CStatement b -> CStatement b
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    DefFun :: (Typeable b) => Int -> CStatement b -> CStatement () -- needed to change this to ()
    DefVar :: (Typeable a) => CExpression a -> CStatement a
    UpdateVar :: (Typeable a) => Int -> CExpression a -> CStatement () 
    While :: CExpression Bool -> CStatement a -> CStatement a

fresh :: State Int Int
fresh = do
  n <- get
  modify (+1)
  return n

data Compiled a = Compiled
  { setup :: CStatement ()   -- | statements to run first (possibly empty)
  , result :: CExpression a  -- | the resulting expression
  }

type Trans a = State Int (Compiled a)

pure' :: CExpression a -> Trans a
pure' expr = return $ Compiled { setup = Return (Val UnitV), result = expr }

toStatement :: Compiled a -> CStatement a
toStatement c = Seq (setup c) (Return (result c))

translate :: forall a. NL.NamedLang a -> State Int (Compiled a)
translate (NL.LInt n)       = pure' (Val (IntV n))
translate (NL.LBool b)      = pure' (Val (BoolV b))
translate (NL.Var n)        = pure' (Var n)
translate (NL.Prod a b) = do 
    ca <- translate a
    cb <- translate b
    return $ Compiled { setup = Seq (setup ca) (setup cb)
    , result = Prod (result ca) (result cb) 
    }
translate (NL.Fst p) = do 
    cp <- translate p
    return $ Compiled
        { setup = setup cp,
        result = Fst (result cp)}
translate (NL.Snd p) = do 
    cp <- translate p
    return $ Compiled
        { setup = setup cp,
        result = Snd (result cp)}
translate (NL.LIntOp op l r) = do
  cl <- translate l
  cr <- translate r
  return $ Compiled 
    { setup = Seq (setup cl) (setup cr)
    , result = LIntOp op (result cl) (result cr) 
    }
translate (NL.LCmpOp op l r) = do
  cl <- translate l
  cr <- translate r
  return $ Compiled 
    { setup = Seq (setup cl) (setup cr)
    , result = LCmpOp op (result cl) (result cr) 
    }
translate (NL.If cond t f) = do
  cc <- translate cond
  ct <- translate t
  cf <- translate f
  n  <- fresh
  let stmt = Bind (If (result cc) (Return (result ct)) (Return (result cf))) n
           $ Return (Var n)
  return $ Compiled { setup = stmt, result = Var n }
translate (NL.Fix (NL.Lam i1 (NL.Lam i2 (NL.If cond base (NL.Apply (NL.Var i) arg))))) 
  | i1 == i = do
    ccond <- translate cond
    cbase <- translate base
    carg <- translate arg
    acc <- fresh
    n <- fresh
    let v_acc = Var acc :: CExpression a
        loop_body = Seq (UpdateVar i2 (result carg)) (UpdateVar acc (result cbase))
        wh = Seq (UpdateVar acc (result cbase)) (While (Not (result ccond)) loop_body)
        stmt = DefFun i2 (Bind (DefVar v_acc) acc wh)
    return $ Compiled { setup = stmt, result = Var n }
translate (NL.Fix f) = do
  cf <- translate f
  n <- fresh
  let stmt = DefFun n (toStatement cf)
  return $ Compiled { setup = stmt, result = Var n }
translate (NL.Lam i f) = do
    cf <- translate f
    let stmt = DefFun i (toStatement cf)
    return $ Compiled { setup = stmt, result = Var i }
translate (NL.Apply f x) = do
    cf <- translate f
    cx <- translate x
    pure' (CallExpr (result cf) (result cx))
-- translate c (NL.Apply f x) = let (f', c') = translate c f
--                                  (x', c'') = translate c' x
--                                in (CallExpr f' x', c'')
-- translate c (NL.Lam i body) = let (body', c') = translate i body
--                                 in (DefFun i body', c')
-- translate c (NL.Fix (NL.Lam i1 (NL.Lam i2 (NL.If cond thn els)))) =
--     let (thn', c') = translate c thn
--         (cond', c'') = translate c' cond
--         (els', c''') = translate c'' els
--         body''      = rewriteRecCall i1 i2 els'
--         v = Var i1 :: CExpression (a -> a)
--     in (DefFun i2 (Bind (DefVar v) c' (Seq (Seq (UpdateVar i1 thn') (While (Not cond') body'')) (Return (Var i1)))), (c' + 1))
-- -- translate c (NL.Fix f) =
-- --     let (body', c')  = translate c f
-- --     in case body' of
-- --         (DefFun i body) -> (CallExpr body' (Var i), c')
-- --         _ -> (CallExpr body' (Var c), c' + 1)
-- -- translate c (NL.Fix (NL.Lam i body)) =
-- --     let (body', c') = translate i body
-- --         funDef = DefFun i body'
-- --     in (Bind funDef c' (CallExpr (Var c') (Var c')), c' + 1)

-- translate c (NL.Fix f) =
--     let (body', c')  = translate c f
--     in (CallExpr body' (Var c), c' + 1)


-- rewriteRecCall :: Int -> Int -> CExpression a -> CExpression a
-- rewriteRecCall f n (CallExpr (Var i) newArg)
--     | i == f = Seq (UpdateVar n (rewriteRecCall f n newArg)) (Var n)
-- rewriteRecCall f n (LIntOp op (CallExpr (Var il) newArgl) (CallExpr (Var ir) newArgr))
--     | il == f && ir == f = let rewriteLeft = rewriteRecCall f n newArgl
--                                rewriteRight = rewriteRecCall f n newArgr
--                                lhs = Seq (UpdateVar f (LIntOp op (Var f) (Var n))) (UpdateVar n rewriteLeft)
--                                rhs = Seq (Seq (UpdateVar f (LIntOp op (Var f) (Var n))) (UpdateVar n rewriteRight)) (Var f)
--                             in Seq lhs rhs
-- rewriteRecCall f n (LIntOp op (CallExpr (Var i) newArg) y)
--     | i == f = Seq (UpdateVar f (LIntOp op (Var f) (rewriteRecCall f n y))) (Seq (UpdateVar n (rewriteRecCall f n newArg)) (Var n))
-- rewriteRecCall f n (LIntOp op x (CallExpr (Var i) newArg))
--     | i == f = Seq (UpdateVar f (LIntOp op (Var f) (rewriteRecCall f n x))) (Seq (UpdateVar n (rewriteRecCall f n newArg)) (Var n))
-- rewriteRecCall self param (LIntOp op x y) = LIntOp op (rewriteRecCall self param x) (rewriteRecCall self param y)
-- rewriteRecCall self param (LCmpOp op x y) = LCmpOp op (rewriteRecCall self param x) (rewriteRecCall self param y)
-- rewriteRecCall self param (If cond thn els) =
--     If (rewriteRecCall self param cond)
--        (rewriteRecCall self param thn)
--        (rewriteRecCall self param els)
-- rewriteRecCall self param (Bind e n body) = Bind e n (rewriteRecCall self param body)
-- rewriteRecCall self param x = x
-- rewriteRecCall i (CallExpr (Var j) arg) | i == j = UpdateVar i arg
-- rewriteRecCall i (If cond thn els) = If cond (rewriteRecCall i thn) (rewriteRecCall i els)
-- rewriteRecCall i (Bind e n body) = Bind e n (rewriteRecCall i body)
-- rewriteRecCall _ e = e

valueToLiteral :: CValue a -> a
valueToLiteral (IntV i) = i
valueToLiteral (BoolV i) = i

-- eval :: CExpression a -> Map Int Dynamic -> (a, Map Int Dynamic)
-- eval (Val x) m = case x of
--         (IntV i) -> (i, m)
--         (BoolV i) -> (i, m)
-- eval (Return x) m = eval x m
-- eval (Var i) m = case Map.lookup i m of
--                         Just dyn -> case fromDynamic dyn of
--                             Just v -> (v,  m)
--                             Nothing -> error "Type mismatch in env"
--                         Nothing -> error "Variable not found"
-- eval (UpdateVar i x) m = let m' = Map.insert i (toDyn x) m
--                          in ((), m')
-- eval (LIntOp op lhs rhs) m = let (lhs', m') = eval lhs m
--                                  (rhs', m'') = eval rhs m'
--                             in ( AL.binop op lhs' rhs', m'')
-- eval (LCmpOp op lhs rhs) m = let (lhs', m') = eval lhs m
--                                  (rhs', m'') = eval rhs m'
--                             in (AL.cmpop op lhs' rhs', m'')
-- eval (CallExpr f arg) m = let (fn, m') = eval f m
--                               (arg', m'') = eval arg m'
--                               res = fn arg'
--                             in (res, m'')
-- eval (Seq x y) m = let (_, m') = eval x m
--                    in eval y m'
-- eval (Bind x i y) m = let (p, m') = eval x m
--                           m'' = Map.insert i (toDyn p) m'
--                         in eval y m''
-- eval (If cond body alt) m = let (cond', m') = eval cond m
--                             in if cond'
--                                then eval body m'
--                                else eval alt m'
-- eval (While cond body) m = let (cond', m') = eval cond m
--                            in if cond'
--                               then let (_, m'') = eval body m'
--                                    in eval (While cond body) m''
--                               else error "While loop has no return value"
-- eval (DefVar x) m = eval x m
-- eval (DefFun i body) m = (\x -> let m' = Map.insert i (toDyn x) m
--                                   in fst (eval body m'), m)
-- eval (Prod l r) m = let (l', m') = eval l m
--                         (r', m'') = eval r m'
--                     in ((l',r'), m'')
-- eval (Fst p) m = let (p', m') = eval p m
--                     in (fst p', m')
-- eval (Snd p) m = let (p', m') = eval p m
--                     in (snd p', m')
-- eval (Not x) m = let (x', m') = eval x m
--                 in (not x', m')
-- eval _ m = error "unkown expr"

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
showCValue UnitV = show ""

showCStmt :: Int -> CStatement a -> Map Int Dynamic -> String
showCStmt indent (UpdateVar i x) m = "\n" ++ indentStr indent ++ "v" ++ show i ++ " =~ " ++ showCExpression indent x m
showCStmt indent (If cond t f) m = 
    "\n" ++ indentStr indent ++ "if " ++ showCExpression indent cond m ++ " {"
    ++  showCStmt (indent + 1) t m 
    ++ "\n" ++ indentStr indent ++ "} else {"
    ++ showCStmt (indent + 1) f m 
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (While cond body) m =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression indent cond m ++ " {"
    ++ showCStmt (indent + 1) body m 
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (Bind x i y) m = 
    "\n" ++ indentStr indent ++ "bind v" ++ show i ++ " = {"
    ++ showCStmt (indent + 1) x m 
    ++ "\n" ++ indentStr indent ++ "} in {" 
    ++ showCStmt (indent + 1) y m 
    ++ "\n" ++  indentStr indent ++ "}"
showCStmt indent (Seq x y) m = 
    showCStmt indent x m ++ showCStmt indent y m
showCStmt indent (DefFun i f) m = 
    "\n" ++ indentStr indent ++ "function (v" ++ show i ++ ") {"
    ++ showCStmt (indent + 1) f m
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent (Return x) m =  "\n" ++ indentStr indent ++ "[Return " ++ showCExpression indent x m ++ "]"
showCStmt indent (DefVar f) m =  "\n" ++ indentStr indent ++ "def " ++ showCExpression indent f m

showCExpression :: Int -> CExpression a -> Map Int Dynamic -> String
showCExpression _ (Var i) _ = "v" ++ show i
showCExpression indent (Not x) m = "!" ++ showCExpression indent x m
showCExpression indent (LIntOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showBinOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression indent (LCmpOp op x y) m = "(" ++ showCExpression indent x m ++ " " ++ showCmpOp op ++ " " ++ showCExpression indent y m ++ ")"
showCExpression _ (Val v) _ = showCValue v
showCExpression indent (CallExpr f arg) m = showCExpression indent f m ++ "(" ++ showCExpression indent arg m ++ ")"
showCExpression indent (Prod l r) m = "(" ++ showCExpression indent l m ++ "," ++ showCExpression indent r m ++ ")"
showCExpression indent (Fst p) m = showCExpression indent p m ++ "[0]"
showCExpression indent (Snd p) m = showCExpression indent p m ++ "[1]"

main :: IO ()
main = do
    let (nl, c') = NL.translate 0 AL.gcdLang
        cl = evalState (translate nl)  c'
        -- (ev, c'') = eval cl Map.empty
    print c'
    putStrLn "--- Translating CL ---"
    putStrLn $ showCStmt 0 (setup cl) Map.empty