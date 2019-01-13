{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Process
import System.FilePath
import System.IO.Temp
import Debug.Trace

import Data.Maybe
import Data.List
import Control.Arrow
import Control.Monad
import Control.Monad.State.Strict
import Text.Read (readMaybe)
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified AbsLatte as T
import PrintProg
import Compile

main :: IO ()
main = hspec $
    it "calculates arithmetic expressions" $ property prop_CalcExpr

data Expr
    = Op Expr Op Expr
    | Const Int
    | Var Name
    | Fun Int Expr

data Op = Plus | Minus | Times | Div | Mod
    deriving Eq

data OptD = Fold | CSE | Copy | Dead
    deriving (Eq, Show)

newtype Name = Name String
    deriving (Eq, Show)

instance Arbitrary Op where
    arbitrary = frequency . map (second pure) $ [(6, Plus), (3, Minus), (0, Times), (1, Div), (1, Mod)]

instance Arbitrary Name where
    arbitrary = elements $ map Name ["a", "b", "c", "d", "e"]

instance Arbitrary Expr where
    arbitrary = choose (10, 15) >>= \s -> uniqueFuns <$> resize s (sized expr)

    shrink (Op a op b) = [a, b] ++ [Op a' op b' | (a', b') <- shrink (a, b)]
    shrink (Fun x e) = [e] ++ [Fun x e' | e' <- shrink e]
    shrink _ = []

instance Show Expr where
    show = \case
        Op a op b    -> "(" ++ show a ++ " " ++ pOp op ++ " " ++ show b ++ ")"
        Var (Name v) -> v
        Const x      -> if x < 0 then "(" ++ show x ++ ")" else show x
        Fun f e      -> "((\\_ -> " ++ show e ++ ") " ++ show f ++ ")"

type FVar = (Int, Name)

toOpt :: OptD -> Opt
toOpt = \case
    Fold -> fold
    CSE  -> cse
    Copy -> copy
    Dead -> cleanDead


expr :: Int -> Gen Expr
expr 0 = frequency $ [(1, Const <$> arbitrary), (1, Var <$> arbitrary)]
expr n | n > 0 = frequency $
    [ (15, Op <$> subexpr <*> arbitrary <*> subexpr)
    , (2, Const <$> arbitrary)
    , (2, Fun 0 <$> subexpr)
    , (4, Var <$> arbitrary)
    ]
  where
    subexpr = expr (n - 1)

uniqueFuns :: Expr -> Expr
uniqueFuns e = evalState (go e) 0
  where
    go :: Expr -> State Int Expr
    go (Op a op b) = Op <$> go a <*> pure op <*> go b
    go (Fun _ e)   = modify (+1) *> (Fun <$> get <*> go e)
    go e           = pure e

genVals :: [FVar] -> Gen [(FVar, Int)]
genVals = mapM (\v -> (v, ) <$> arbitrary)

vars :: Expr -> [FVar]
vars = vars' 0

vars' :: Int -> Expr -> [FVar]
vars' f = \case
    Op a _ b -> vars' f a `union` vars' f b
    Const _  -> []
    Var v    -> [(f, v)]
    Fun f' e -> vars' f' e

depth :: Expr -> Int
depth = \case
    Op a _ b -> 1 + max (depth a) (depth b)
    Fun _ e  -> depth e
    _        -> 0

hasDivideByZero :: [(FVar, Int)] -> Expr -> Bool
hasDivideByZero = hasDivideByZero' 0

hasDivideByZero' :: Int -> [(FVar, Int)] -> Expr -> Bool
hasDivideByZero' f vs = \case
    Op a op b ->
        hasDivideByZero' f vs a ||
        hasDivideByZero' f vs b ||
        ((op == Div || op == Mod) && (eval' f vs b == 0))
    Fun f' e -> hasDivideByZero' f' vs e
    _ -> False

pOp :: Op -> String
pOp = \case
    Plus  -> "+"
    Minus -> "-"
    Times -> "*"
    Div   -> "/"
    Mod   -> "%"

eval :: [(FVar, Int)] -> Expr -> Int
eval = eval' 0

eval' :: Int -> [(FVar, Int)] -> Expr -> Int
eval' f vs = \case
    Op a op b -> toFun op (eval' f vs a) (eval' f vs b)
    Var v     -> vs `at` (f, v)
    Const x   -> x
    Fun f' e  -> eval' f' vs e
  where
    toFun = \case
        Plus  -> (+)
        Minus -> (-)
        Times -> (*)
        Div   -> quot
        Mod   -> rem

    at :: Eq a => [(a, b)] -> a -> b
    at l i = fromJust $ lookup i l

funAsmName :: Int -> String
funAsmName i = "f" ++ show i

absExpr :: Expr -> T.Expr Pos
absExpr = \case
    Op a op b | elem op [Plus, Minus] -> T.EAdd pos (absExpr a) (absAdd op) (absExpr b)
    Op a op b                         -> T.EMul pos (absExpr a) (absMul op) (absExpr b)
    Const x                           -> T.ELitInt pos $ fromIntegral x
    Var (Name v)                      -> T.EVar pos (T.Ident v)
    Fun f _                           -> T.EApp pos (T.Ident $ funAsmName f) []
  where

absMul :: Op -> T.MulOp Pos
absMul = \case
    Times -> T.Times pos
    Div   -> T.Div pos
    Mod   -> T.Mod pos

absAdd :: Op -> T.AddOp Pos
absAdd = \case
    Plus  -> T.Plus pos
    Minus -> T.Minus pos

pos :: Maybe (Int, Int)
pos = Nothing

collectFuns :: Expr -> [(Int, Expr)]
collectFuns = \case
    Op a _ b -> collectFuns a ++ collectFuns b
    Fun f e  -> [(f, e)]
    _        -> []

simpleProgram :: Expr -> [(FVar, Int)] -> T.Program Pos
simpleProgram e vs = T.Program pos $
    [ T.FnDef pos (T.Int pos) (T.Ident "main") [] $ T.Block pos $
        map (uncurry define) (varsFor 0 vs) ++
        [ T.SExp pos $ T.EApp pos (T.Ident "printInt") [absExpr e]
        , T.Ret pos $ T.ELitInt pos 0
        ]
    ] ++ concatMap (uncurry funDef) (collectFuns e)
  where
    varsFor :: Int -> [(FVar, Int)] -> [(Name, Int)]
    varsFor f = map (first snd) . filter ((== f) . fst . fst)

    define :: Name -> Int -> T.Stmt Pos
    define (Name v) x = T.Decl pos (T.Int pos) [T.Init pos (T.Ident v) $ T.ELitInt pos $ fromIntegral x]

    funDef :: Int -> Expr -> [T.TopDef Pos]
    funDef f e =
        [ T.FnDef pos (T.Int pos) (T.Ident $ funAsmName f) [] $ T.Block pos $
            map (uncurry define) (varsFor f vs) ++
            [ T.Ret pos $ absExpr e ]
        ] ++ concatMap (uncurry funDef) (collectFuns e)

runCode :: String -> IO String
runCode code = withSystemTempDirectory "quickcheck_latc" $ \dir -> do
    let execPath = dir </> "prog"
        objPath = execPath <.> "o"
        asmPath = execPath <.> "s"
    writeFile asmPath code
    runAs asmPath objPath
    runLd objPath execPath
    readProcess execPath [] ""

prop_CalcExpr :: Expr -> Property
prop_CalcExpr e =
    collect (depth e) $
    forAll (genVals $ vars e) $ \vs ->
    not (hasDivideByZero vs e) && depth e > 1 ==>
    let p = simpleProgram e vs in counterexample (pProg p) $
    let expected = eval vs e in counterexample ("expected: " ++ show expected) $
    forAll (resize 12 $ listOf $ elements [Fold, CSE, Copy, Dead]) $ \opts ->
    collect opts $
    monadicIO $ do
        --traceM $ "testing expr: " ++ show e
        prog <- case semantic p of
            Left err -> fail err
            Right p  -> pure p
        let (quads, asm) = Compile.generate prog $ map toOpt opts
        --traceM $ "asm:\n" ++ code
        out <- run $ runCode asm
        let out' = readMaybe out
        monitor $ counterexample $ "got: " ++ show out'
        monitor $ counterexample $ "quads:\n" ++ quads
        monitor $ counterexample $ "asm:\n" ++ asm
        assert $ out' == Just expected
