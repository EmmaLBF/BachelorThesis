{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- HLINT ignore "Use first" -}

module CLang where
import Data.Dynamic
import AbsLang (BinOp(..), CmpOp(..))
import qualified AbsLang as AL
import qualified NamedLang as NL
import qualified FirstOrderLang as FOL

data Value a where
    IntV :: Int -> Value Int
    BoolV :: Bool -> Value Bool
    FuncV :: (Value a -> Value b) -> Value (a -> b)

data Expr a where
    


data Statement a where
    Return :: Value a -> ()
    Seq :: Statement a -> (Value a -> Statement b) -> Statement b
    If :: Expr Bool -> Statement a -> Statement a
    Def :: (Value a -> Statement b) -> Value (a -> b)
    While ::


-- data CType
--     = Int
--     | Bool
--     deriving (Show, Eq)

-- data CExpr
--     = LInt Int
--     | LBool Bool
--     | Var Int
--     | LIntOp AL.BinOp CExpr CExpr
--     | LCmpOp AL.CmpOp CExpr CExpr
--     | Prod CExpr CExpr
--     | Fst CExpr
--     | Snd CExpr
--     | Fix CFunction
--     deriving (Show)

-- data CStmt
--     = If CExpr [CStmt] [CStmt]
--     | App CFunction [CExpr]
--     deriving (Show)

-- data CFunction
--     = CFunction {
--         id :: Int,
--         returnType :: CType,
--         params :: [(CType, Int)], -- INt? should it be CExpr ?
--         body :: [CStmt]
--     } deriving (Show)


-- lambda in cpp
-- auto add = [](int a, int b) -> int {
--         return a + b;
--     };