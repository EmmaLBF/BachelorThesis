{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang where
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Reader (ReaderT (..), MonadReader (ask))

import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    FuncV :: (CValue a -> CValue b) -> CValue (a -> b)

data CExpression a where
    Var :: Int -> CExpression a
    LIntOp :: AL.BinOp -> CExpression Int -> CExpression Int -> CExpression Int
    LCmpOp :: AL.CmpOp -> CExpression Int -> CExpression Int -> CExpression Bool
    Val :: CValue a -> CExpression a

data CStatement a where
    Return :: CExpression a -> CStatement a
    Seq :: CStatement a -> (CValue a -> CStatement b) -> CStatement b
    If :: CExpression Bool -> CStatement a -> CStatement a -> CStatement a
    Def :: (CValue a -> Either String (CStatement b)) -> CStatement (a -> b)
    Fix :: CStatement (a -> a) -> CStatement a
    While :: CExpression Bool -> CStatement a -> CStatement a -- condition + body
    Call :: CStatement (a -> b) -> CExpression a -> CStatement b

type TransM a = ReaderT (Map Int Dynamic) (Either String) a

translateExpr :: NL.NamedLang a -> TransM (CExpression a)
translateExpr (NL.Var x) = Right (Var x)
translateExpr (NL.LInt x) = Right (Val (IntV x))
translateExpr (NL.LBool x) = Right (Val (BoolV x))
translateExpr (NL.LIntOp x y z) = do p <- translateExpr y
                                     q <- translateExpr z
                                     return (LIntOp x p q)
translateExpr (NL.LCmpOp x y z) = do p <- translateExpr y
                                     q <- translateExpr z
                                     return (LCmpOp x p q)
translateExpr _ = Left "expected expression, got statement"


translateStmt :: NL.NamedLang a -> TransM (CStatement a)
translateStmt (NL.Lam i body) = do  m <- ask
                                    return (Def (\x -> runReaderT (translateStmt body) (Map.insert i (toDyn x) m)))
translateStmt (NL.Fix f) = do p <- translateStmt f
                              return (Fix p)
translateStmt (NL.Apply x y) = do x' <- translateStmt x
                                  y' <- translateExpr y
                                  return (Call x' y')
translateStmt (NL.If x y z) = do p <- translateExpr x
                                 q <- translateStmt y
                                 r <- translateStmt z
                                 return (If p q r)
translateStmt x = do p <- translateExpr x 
                     return (Return p)
-- translateStmt (NL.Prod x y) = ()
-- translateStmt (NL.Fst x) = ()
-- translateStmt (NL.Snd x) = ()


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
showCValue (FuncV _) = "<func>"

showCExpression :: CExpression a -> String
showCExpression (Var i) = "v" ++ show i
showCExpression (LIntOp op x y) = "(" ++ showCExpression x ++ " " ++ showBinOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (LCmpOp op x y) = "(" ++ showCExpression x ++ " " ++ showCmpOp op ++ " " ++ showCExpression y ++ ")"
showCExpression (Val v) = showCValue v

showCStatement :: CStatement a -> String
showCStatement (Return e) = "return " ++ showCExpression e
showCStatement (Seq s _) = "seq (" ++ showCStatement s ++ ") <function>"
showCStatement (If c t e) = "if " ++ showCExpression c ++ " then " ++ showCStatement t ++ " else " ++ showCStatement e
showCStatement (Def _) = "def <function>"
showCStatement (Fix s) = "fix (" ++ showCStatement s ++ ")"
showCStatement (While c b) = "while " ++ showCExpression c ++ " do " ++ showCStatement b
showCStatement (Call f x) = showCExpression f ++ " (" ++ showCExpression x ++ ")"

main :: IO ()
main = do
  putStrLn "--- Translating FO ---"
  putStrLn $ "Factorial 5 = " ++ case translateStmt (fst (NL.translate 0 AL.fac)) of
    (Left err) -> show err
    (Right val) -> showCStatement val
 