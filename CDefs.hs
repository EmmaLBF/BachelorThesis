{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module CDefs where
    
import CLang (indentStr)
import qualified AbsLang as AL
import qualified CLang as CL

import Data.Typeable
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

data CParam where
  CParam :: Int -> CType -> CParam
  CParamEnv  :: Int -> CParam -- void* env parameter
  deriving (Show, Ord)
instance Eq CParam where
  CParam i _ == CParam j _ = i == j
  CParamEnv i == CParamEnv j = i == j
  CParam i _ == CParamEnv j = i == j
  CParamEnv i == CParam j _ = i == j

type CParams = [CParam]
type CParamMap = Map.Map Int CParam

data CType
    = CTInt
    | CTBool
    | CTVoid
    | CTNode
    | CTNodeInt
    | CTNodeBool
    | CTPair CType CType
    | CTClosure
    | CTPtr CType
    | CTFun CType CType
    | CTVoidPtr
    deriving (Show, Eq, Ord)

data CArg where
  CArg :: CType -> CExpression -> CArg

type CArgMap = Map.Map Int CArg

data CValue a where
    IntV :: Int -> CValue Int
    BoolV :: Bool -> CValue Bool
    UnitV :: CValue ()
    FunV  :: (CValue a -> CValue b) -> CValue (a -> b)
    PairV :: CValue a -> CValue b -> CValue (a, b)
    ListV :: [CValue a] -> CValue [a]
    ClosureV :: Int -> CValue a
    EnvV :: Int -> CValue a

data CExpression where
    Val :: CValue a -> CExpression
    Not :: CExpression -> CExpression
    Abs :: CExpression -> CExpression
    Var :: CType -> Int -> CExpression
    LIntOp :: AL.BinOp -> CExpression -> CExpression -> CExpression
    LCmpOp :: AL.CmpOp -> CExpression -> CExpression -> CExpression
    LBoolOp :: AL.BoolOp -> CExpression -> CExpression -> CExpression
    Ternary :: CType -> CExpression -> CExpression -> CExpression -> CExpression
    -- Tuples
    Prod :: CType -> CExpression -> CExpression -> CExpression
    Fst :: CType -> CType -> CExpression -> CExpression -- holds type of res (a) and whole pair (a,b)
    Snd :: CType -> CType -> CExpression -> CExpression
    -- Lists
    EmptyList :: CType -> CExpression
    ConsList :: CType -> CExpression -> CExpression -> CExpression
    HeadList :: CType -> CExpression -> CExpression
    TailList :: CType -> CExpression -> CExpression
    IsEmpty :: CType -> CExpression -> CExpression
    IndexList :: CType -> CExpression -> CExpression -> CExpression
    -- Lambda
    ApplyClosure :: CType -> CExpression -> CExpression -> CExpression  -- apply(f, arg), type of arg passed
    GetEnvField :: CType -> Int -> Int -> CExpression  -- ((Env_vN*)env)->vM, with type for cast
    CallExpr :: CType -> CType -> CExpression -> CExpression -> CExpression
    -- Casting
    CastExpr :: CType -> CExpression -> CExpression
    Box :: CType -> CExpression -> CExpression
    Unbox :: CType -> CExpression -> CExpression

data CStatement where
    Return :: CExpression -> CStatement
    Seq :: CStatement -> CStatement -> CStatement
    If :: CExpression -> CStatement -> CStatement -> CStatement
    DefFun :: CType -> Int -> CParams -> CStatement -> CStatement
    DefVar :: CType -> Int -> CExpression -> CStatement
    UpdateVar :: CType -> Int -> CExpression -> CStatement
    While :: CExpression -> CStatement -> CStatement
    Skip :: CStatement
    DefEnvStruct :: Int -> CParams -> CStatement -- same, but fields are concrete types
    AllocClosure :: Int -> CStatement -- closureId
    AllocEnv :: Int -> Int -> CArgMap -> CArgMap -> CStatement -- envId parentId directParams parentParams


instance Eq CExpression where
    l == r = showCExpression l Map.empty == showCExpression r Map.empty
instance Eq CStatement where
    l == r = showCStmt 0 Map.empty l == showCStmt 0 Map.empty r
instance Show CArg where
    show (CArg _ x) = showCExpression x Map.empty


-- maps a function id (that returns a closure) to the function said closure contains
type ClosureFuns = Map.Map Int Int
type ClosureParams = Set.Set Int -- set of params which are closures
-- maps a function id to the number of new parameters it has (for later printing calls/apply corretcly)
type MergedMap = Map.Map Int Int
-- maps a function id to the set of Cparams of all of the functions it is nested in
type FreeVars = Map.Map Int (Set.Set CParam)

data GlobalInfo = GlobalInfo
    { usedEnvs :: Set.Set Int   -- var ids that flow into heap
    , closureUses :: Map.Map Int Int -- id of closure -> number of times used
    , functionCallsGlobal :: Map.Map Int Int -- id of function called -> number of times called
    , globalUsedVars :: Set.Set Int
    , aliases :: Map.Map Int CArg
    , callArgs :: Map.Map Int [[CArg]]
    , pairTypes :: Set.Set (CType, CType)
    } deriving (Show)

data FunctionInfo = FunctionInfo
    {
      funId :: Int
    , funParams :: CParams
    , escapedVars :: Set.Set Int   -- var ids that flow into heap
    , varUses :: Map.Map Int Int
    , varDefs :: Map.Map Int CArg
    , escapedEnvs :: Set.Set Int   -- env ids that outlive the frame
    , allocedEnvs :: Set.Set Int
    , envUses :: Set.Set Int
    , functionCalls :: Map.Map Int Int
    , escapedClos :: Set.Set Int
    } deriving (Show)

-- HELPERS

collectArgs :: CExpression -> (CExpression, [CArg])
collectArgs (CallExpr _ tx f x) =
    let (f', as) = collectArgs f
    in (f', as ++ [CArg tx x])
collectArgs e = (e, [])

collectArgsApply :: CExpression -> (CExpression, [CArg])
collectArgsApply (ApplyClosure tx f x) =
    let (f', as) = collectArgsApply f
    in (f', as ++ [CArg tx x])
collectArgsApply e = (e, [])

-- SHOW

showCArg :: CArg -> MergedMap -> String
showCArg (CArg _ x) = showCExpression x

-- convert haskell type to my CType
fromTypeRep :: TypeRep -> CType
fromTypeRep p =
    let args = typeRepArgs p
        con = show (typeRepTyCon p)
    in case (con, args) of
        ("Int", []) -> CTInt
        ("Bool", []) -> CTBool
        ("()", []) -> CTVoid
        ("[]", [a]) | show a == "Int" -> CTNodeInt
                    | show a == "Bool" -> CTNodeBool
        ("[]", [_]) -> CTNode
        ("(,)", [l,r]) -> CTPtr (CTPair (fromTypeRep l) (fromTypeRep r))
        ("->", [a, b]) -> CTFun (fromTypeRep a) (fromTypeRep b)
        _  -> CTVoidPtr

printPairType :: CType -> String
printPairType x = case x of
    CTInt -> "Int"
    CTBool -> "Bool"
    CTVoid -> "Void"
    CTNode -> "Node"
    CTNodeInt -> "NodeInt"
    CTNodeBool -> "NodeBool"
    CTPair _ _ -> "Pair"
    CTClosure -> "CLosure"
    CTPtr ct -> printPairType ct ++ "Ptr"
    CTFun ct ct' -> "Fun" ++ printPairType ct ++ printPairType ct'
    CTVoidPtr -> "VoidPtr"

-- print type for var decl, give string "v" + id
printDecl :: String -> CType -> String
printDecl name CTInt = "int " ++ name
printDecl name CTBool = "bool " ++ name
printDecl name CTVoid = "void* " ++ name
printDecl name CTNode = "Node* " ++ name
printDecl name CTNodeInt = "NodeInt* " ++ name
printDecl name CTNodeBool = "NodeBool* " ++ name
printDecl name (CTPair tl tr) = "Pair_" ++ printPairType tl ++ "_" ++ printPairType tr ++ " " ++ name
printDecl name CTClosure = "Closure* " ++ name
printDecl name CTVoidPtr = "void* " ++ name
printDecl name (CTPtr t) = printDecl ("*" ++ name) t
printDecl name (CTFun a b) = printFunPtr name a b

-- print type for function pointers, need to have recursive ()* with args
printFunPtr :: String -> CType -> CType -> String
printFunPtr name arg ret =
    case ret of
        CTFun a2 b2 -> printFunPtr ("(*" ++ name ++ ")(" ++ printType arg ++ ")") a2 b2
        _ -> printType ret ++ " (*" ++ name ++ ")(" ++ printType arg ++ ")"

printType :: CType -> String
printType CTInt = "int"
printType CTBool = "bool"
printType CTVoid = "void"
printType CTNode = "Node*"
printType CTNodeInt = "NodeInt*"
printType CTNodeBool = "NodeBool*"
printType (CTPair tl tr) = "Pair_" ++ printPairType tl ++ "_" ++ printPairType tr
printType CTClosure = "Closure*"
printType CTVoidPtr = "void*"
printType (CTPtr t) = printType t ++ "*"
printType (CTFun a b) = printFunPtr "" a b

showCParams :: CParams -> String
showCParams params =
    let hasEnv = any isEnv params
    in intercalate ", " (map (showParam hasEnv) params)
  where
    isEnv (CParamEnv _) = True
    isEnv _ = False
    showParam _ (CParamEnv i) = "void* env" ++ show i
    showParam True (CParam i _) = "void* v" ++ show i ++ "_raw" -- closure function, use void*
    showParam False (CParam i t) = printDecl ("v" ++ show i) t  -- plain function, keep type

showProxFunc :: String -> CParams -> CType -> String
showProxFunc name params (CTFun arg ret) =
    printType ret ++ " (*" ++ name ++ "(" ++ showCParams params ++ "))(" ++ printType arg ++ ")"
showProxFunc name params ct =
    printType ct ++ " " ++ name ++ "(" ++ showCParams params ++ ")"

box :: CType -> String -> String
box CTInt  e = "box_int(" ++ e ++ ")"
box CTBool e = "box_bool(" ++ e ++ ")"
box _      e = e

unbox :: CType -> String -> String
unbox CTInt  e = "*(int*)" ++ e
unbox CTBool e = "*(bool*)" ++ e
unbox (CTPair l r) e = "*(Pair_" ++ printPairType l ++ "_" ++ printPairType r ++ "*)" ++ e
unbox t      e = "(" ++ printType t ++ ")" ++ e

boxForApply :: CType -> String -> String
boxForApply CTInt  e = "box_int(" ++ e ++ ")"
boxForApply CTBool e = "box_bool(" ++ e ++ ")"
boxForApply _      e = "(void*)(" ++ e ++ ")"

showCValue :: CValue a -> String
showCValue (IntV n)  = show n
showCValue (BoolV b) = if b then "true" else "false"
showCValue UnitV = "NULL"
showCValue (PairV x y) =
    "{ .fst = " ++ showCValue x ++ ", .snd = " ++ showCValue y ++ "}"
showCValue (FunV _) = "NULL"
showCValue (ListV l) =
  case l of
    [] -> ""
    (h:t) -> showCValue h ++ ", " ++ showCValue (ListV t)
showCValue (ClosureV i) = "c" ++ show i
showCValue (EnvV i) = "env" ++ show i

showListLibFunType :: CType -> String
showListLibFunType CTInt = "Int"
showListLibFunType CTBool = "Bool"
showListLibFunType _ = ""

showCExpression :: CExpression -> MergedMap -> String
showCExpression (EmptyList _) _ = "NULL"
showCExpression (Val v) _ = showCValue v
showCExpression (Var _ i) _ = "v" ++ show i
showCExpression (Abs x) m = "abs(" ++ showCExpression x m ++ ")"
showCExpression (Not x) m = "!(" ++ showCExpression x m ++ ")"
showCExpression (LIntOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBinOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (LBoolOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showBoolOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (LCmpOp op x y) m = "(" ++ showCExpression x m ++ " " ++ CL.showCmpOp op ++ " " ++ showCExpression y m ++ ")"
showCExpression (Box t x) m = box t (showCExpression x m)
showCExpression (Unbox t x) m = unbox t ("(" ++ showCExpression x m ++ ")")
showCExpression (Prod t l r) m =
    case t of
        CTPair tl tr -> "(" ++ "Pair_" ++ printPairType tl ++ "_" ++ printPairType tr ++ "){ .fst = " ++ showCExpression l m ++ ", .snd = " ++ showCExpression r m ++ " }"
        CTPtr (CTPair tl tr) -> "makePair_" ++ printPairType tl ++ "_" ++ printPairType tr ++ "(" ++ showCExpression l m ++ ", " ++ showCExpression r m ++ ")"
        _ -> error "not valid type " ++ printType t
showCExpression (Fst tp _ p) m = "(" ++ showCExpression p m ++
    case tp of
        CTPair _ _ -> ").fst"
        _ -> ")->fst"
showCExpression (Snd tp _ p) m = "(" ++ showCExpression p m ++
    case tp of
        CTPair _ _ -> ").snd"
        _ -> ")->snd"
showCExpression (IsEmpty _ l) m = "((" ++ showCExpression l m ++ ") == NULL)"
showCExpression (HeadList _ l) m = "(" ++ showCExpression l m ++ ")->head"
showCExpression (TailList _ l) m = "(" ++ showCExpression l m ++ ")->tail"
showCExpression (ConsList t x l) m = "cons" ++ showListLibFunType t ++ "(" ++ showCExpression x m ++ ", " ++ showCExpression l m ++ ")"
showCExpression (IndexList _ l i) m = showCExpression l m ++ "[" ++ showCExpression i m ++ "]"
showCExpression (Ternary _ cond thn els) m = "((" ++ showCExpression cond m ++ ") ? (" ++ showCExpression thn m ++ ") : (" ++ showCExpression els m ++ "))"
showCExpression (GetEnvField _ structId fieldId) _ = "((Env_v" ++ show structId ++ "*)env" ++ show structId ++ ")->v" ++ show fieldId
showCExpression (CastExpr t x) m = case t of
    CTInt -> "(int)(intptr_t)" ++ showCExpression x m
    CTBool -> "(bool)(intptr_t)" ++ showCExpression x m
    _ -> "(" ++ printType t ++ ")" ++ showCExpression x m
showCExpression (ApplyClosure targ f arg) m =
    let (func, args) = collectArgsApply (ApplyClosure targ f arg)
        applyCall expr argList =
            "(" ++ expr ++ ")->fn(" ++
            intercalate ", " (("(" ++ expr ++ ")->env") : map (`showCArg` m) argList) ++ ")"
        n = case func of
            Var _ i -> Map.findWithDefault 1 i m
            Val (ClosureV i) -> Map.findWithDefault 1 i m
            _ -> 1
        (merged, rest) = splitAt n args
        baseCall = applyCall (showCExpression func m) merged
    in foldl (\acc arg' -> applyCall acc [arg']) baseCall rest
-- merges together nested calls if I merged together the params earlier
showCExpression (CallExpr tf tx f arg) m =
    let (func, args) = collectArgs (CallExpr tf tx f arg)
        formatArgs [] = []
        formatArgs (CArg _ (Val (EnvV j)) : rest) = ("env" ++ show j) : map (\(CArg t' a) ->
                boxForApply t' (showCExpression a m)) rest
        formatArgs args' = map (`showCArg` m) args'
    in case func of
        Var _ i -> case Map.lookup i m of
            Just n ->
                let (merged, rest) = Prelude.splitAt n args
                    baseCall = showCExpression func m ++ "(" ++ intercalate ", " (formatArgs merged) ++ ")"
                in if Prelude.null rest
                   then baseCall
                   else baseCall ++ "(" ++ intercalate ", " (formatArgs rest) ++ ")"
            Nothing -> foldl (\acc a -> acc ++ "(" ++ head (formatArgs [a]) ++ ")") (showCExpression func m) args
        _ -> foldl (\acc a -> acc ++ "(" ++ head (formatArgs [a]) ++ ")") (showCExpression func m) args

showCStmt :: Int -> MergedMap -> CStatement -> String
showCStmt indent m (UpdateVar _ i x) = "\n" ++ indentStr indent ++ "v" ++ show i ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m (If cond t f) =
    case t of
        Return{} -> 
            "\n" ++ indentStr indent ++ "if (" ++ showCExpression cond m ++ ") " ++ dropWhile (== '\n') ( showCStmt 0 m t)
            ++ showCStmt indent m f
        _ -> 
            "\n" ++ indentStr indent ++ "if (" ++ showCExpression cond m ++ ") {"
            ++  showCStmt (indent + 1) m t
            ++ "\n" ++ indentStr indent  ++ "} else {"
            ++ showCStmt (indent + 1) m f
            ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m (While cond body) =
    "\n" ++ indentStr indent ++ "while " ++ showCExpression cond m ++ " {"
    ++ showCStmt (indent + 1) m body
    ++ "\n" ++ indentStr indent ++ "}"
showCStmt indent m (Seq x y) = showCStmt indent m x ++ showCStmt indent m y
showCStmt indent m (DefFun ct ifun params body) =
    let hasEnv = any (\case CParamEnv _ -> True; _ -> False) params
        unboxings = if hasEnv
            then concatMap (\case
                CParam ip t ->
                    "\n" ++ indentStr (indent+1) ++
                    printDecl ("v" ++ show ip) t ++
                    " = " ++ unbox t ("v" ++ show ip ++ "_raw") ++ ";"
                _ -> "") params
            else ""
    in "\n" ++ indentStr indent ++ showProxFunc ("v" ++ show ifun) params ct ++ " {"
    ++ unboxings
    ++ showCStmt (indent + 1) m body
    ++ "\n" ++ indentStr indent ++ "}\n"
showCStmt indent m (DefVar ct i x) =
    "\n" ++ indentStr indent ++
        case x of
            (Val (ClosureV _)) -> printDecl ("c" ++ show i) ct
            (Val (EnvV _)) -> printDecl ("env" ++ show i) ct
            _ ->
                case ct of
                    CTClosure -> printDecl ("c" ++ show i) ct
                    _ -> printDecl ("v" ++ show i) ct
    ++ " = " ++ showCExpression x m ++ ";"
showCStmt indent m (Return x) =  "\n" ++ indentStr indent ++ "return " ++ showCExpression x m ++ ";"
showCStmt indent _ (DefEnvStruct ifun p) =
    "\n" ++ indentStr indent ++ "typedef struct {\n"
    ++ concatMap (\case CParam ip tp -> "    " ++ printDecl ("v" ++ show ip) tp ++ ";\n"; _ -> "") p
    ++ "} Env_v" ++ show ifun ++ ";\n"
showCStmt indent _ (AllocClosure ifun) =
    "\n" ++ indentStr indent ++ "Closure* c" ++ show ifun ++ " = malloc(sizeof(Closure));"
    ++ "\n" ++ indentStr indent ++ "c" ++ show ifun ++ "->env = env" ++ show ifun ++ ";"
    ++ "\n" ++ indentStr indent ++ "c" ++ show ifun
    ++ "->fn = (void* (*)(void*, void*))v" ++ show ifun ++ ";"
showCStmt indent m (AllocEnv envId _ directParams parentParams) =
    "\n" ++ indentStr indent ++ "Env_v" ++ show envId ++ "* env" ++ show envId
        ++ " = malloc(sizeof(Env_v" ++ show envId ++ "));"
    ++ showDirect (Map.toList directParams)
    ++ showDirect (Map.toList parentParams)
  where
    showDirect :: [(Int, CArg)] -> String
    showDirect [] = ""
    showDirect [(ip, CArg _ x)] =
        "\n" ++ indentStr indent ++ "env" ++ show envId ++ "->v" ++ show ip ++ " = " ++ showCExpression x m ++ ";"
    showDirect (i : rest) = showDirect [i] ++ showDirect rest
showCStmt _ _ Skip = ""

showFunDefs :: [CStatement] -> String
showFunDefs [] = ""
showFunDefs [DefFun tret ifun params _] = "\n" ++ showProxFunc ("v" ++ show ifun) params tret ++ ";"
showFunDefs (i:is) = showFunDefs[i] ++ showFunDefs is
