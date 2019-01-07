{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Process
import System.FilePath
import System.IO.Temp
import System.Exit
import System.Timeout
import Debug.Trace

import Data.Maybe
import Data.List
import Control.Arrow
import Control.Monad
import Text.Read
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified AbsLatte as T
import qualified Semantic.Program as S
import qualified Intermediate.Generate as I
import qualified Quad as Q
import qualified Asm.Generate as A
import Intermediate.Flow
import Intermediate.Liveness
import Intermediate.Reaching
import Intermediate.CSE
import PrintProg

main :: IO ()
main = hspec $
    it "calculates arithmetic expressions" $ property prop_CalcExpr

data Expr
    = Op Expr Op Expr
    | Const Int
    | Var Name
    | ConstF ConstFun

data Op = Plus | Minus | Times | Div | Mod
    deriving Eq

newtype Name = Name String
    deriving (Eq, Show)

newtype ConstFun = ConstFun Int
    deriving Eq

instance Arbitrary Op where
    arbitrary = frequency . map (second pure) $ [(6, Plus), (2, Minus), (0, Times), (1, Div), (1, Mod)]

instance Arbitrary Name where
    arbitrary = oneof $ map (pure . Name) ["a", "b", "c", "d", "e"]

instance Arbitrary Expr where
    arbitrary = choose (10, 15) >>= \s -> resize s $ sized expr

    shrink (Op a op b) = [a, b] ++ [Op a' op b' | (a', b') <- shrink (a, b)]
    shrink _ = []

instance Show Expr where
    show = \case
        Op a op b    -> "(" ++ show a ++ " " ++ pOp op ++ " " ++ show b ++ ")"
        Var (Name v) -> v
        Const x      -> if x < 0 then "(" ++ show x ++ ")" else show x
        ConstF (ConstFun x) -> "((\\_ -> " ++ show x ++ ") ())"

expr :: Int -> Gen Expr
expr 0 = frequency $ [(1, genConstF), (1, genInt), (2, Var <$> arbitrary)]
expr n | n > 0 = frequency $
    [ (15, Op <$> subexpr <*> arbitrary <*> subexpr)
    , (2, genInt)
    , (2, genConstF)
    , (4, Var   <$> arbitrary)
    ]
  where
    subexpr = expr (n - 1)

genInt :: Gen Expr
genInt = (\i -> if i < 0 then Op (Const 0) Minus (Const i) else (Const i)) <$> arbitrary

genConstF :: Gen Expr
genConstF = ConstF . ConstFun <$> arbitrary

genVals :: [Name] -> Gen [(Name, Int)]
genVals = mapM (\v -> (v, ) <$> arbitrary)

vars :: Expr -> [Name]
vars = \case
    Op a _ b -> vars a `union` vars b
    Const _  -> []
    Var v    -> [v]
    ConstF _ -> []

depth :: Expr -> Int
depth = \case
    Op a _ b -> 1 + max (depth a) (depth b)
    _        -> 0

hasDivideByZero :: Expr -> [(Name, Int)] -> Bool
hasDivideByZero e vs = case e of
    Op a op b ->
        hasDivideByZero a vs ||
        hasDivideByZero b vs ||
        ((eval b vs == 0) && (op == Div || op == Mod))
    _ -> False

pOp :: Op -> String
pOp = \case
    Plus  -> "+"
    Minus -> "-"
    Times -> "*"
    Div   -> "/"
    Mod   -> "%"

eval :: Expr -> [(Name, Int)] -> Int
eval e vs = case e of
    Op a op b           -> toFun op (eval a vs) (eval b vs)
    Var v               -> vs `at` v
    Const x             -> x
    ConstF (ConstFun x) -> x
  where
    toFun = \case
        Plus  -> (+)
        Minus -> (-)
        Times -> (*)
        Div   -> quot
        Mod   -> rem

type Pos = Maybe (Int, Int)

funAsmName :: ConstFun -> String
funAsmName (ConstFun x) = if x < 0 then "f__" ++ show (-x) else "f_" ++ show x

toAbsExpr :: Expr -> T.Expr Pos
toAbsExpr = \case
    Op a op b | elem op [Plus, Minus] -> T.EAdd pos (toAbsExpr a) (toAbsAddOp op) (toAbsExpr b)
    Op a op b                         -> T.EMul pos (toAbsExpr a) (toAbsMulOp op) (toAbsExpr b)
    Const x                           -> T.ELitInt pos $ fromIntegral x
    Var (Name v)                      -> T.EVar pos (T.Ident v)
    ConstF f@(ConstFun x)               -> T.EApp pos (T.Ident $ funAsmName f) []
  where

toAbsMulOp :: Op -> T.MulOp Pos
toAbsMulOp = \case
    Times -> T.Times pos
    Div   -> T.Div pos
    Mod   -> T.Mod pos

toAbsAddOp :: Op -> T.AddOp Pos
toAbsAddOp = \case
    Plus  -> T.Plus pos
    Minus -> T.Minus pos

pos :: Maybe (Int, Int)
pos = Nothing

collectFuns :: Expr -> [ConstFun]
collectFuns = go
  where
    go (Op a _ b) = go a `union` go b
    go (ConstF x) = [x]
    go _          = []

simpleProgram :: Expr -> [(Name, Int)] -> T.Program Pos
simpleProgram e vs = T.Program pos $
    [ T.FnDef pos (T.Int pos) (T.Ident "main") [] $ T.Block pos $
        map (uncurry define) vs ++
        [ T.SExp pos $ T.EApp pos (T.Ident "printInt") [toAbsExpr e]
        , T.Ret pos $ T.ELitInt pos 0
        ]
    ] ++ map funDef (collectFuns e)
  where
    define :: Name -> Int -> T.Stmt Pos
    define (Name v) x = T.Decl pos (T.Int pos) [T.Init pos (T.Ident v) $ T.ELitInt pos $ fromIntegral x]

    funDef :: ConstFun -> T.TopDef Pos
    funDef f@(ConstFun x) = T.FnDef pos (T.Int pos) (T.Ident $ funAsmName f) [] $
        T.Block pos [ T.Ret pos $ T.ELitInt pos $ fromIntegral x ]

compile :: T.Program Pos -> Either String String
compile p = do
    prog <- left (intercalate "\n") $ S.runProgram p
    let Q.Program consts ds = I.program prog
        (retss, names, argss, qss) = unzip4 $ flip map ds $ \(Q.FunDef rets name args qs) -> (rets, name, args, qs)
        graphs = map mkGraph qss
        bss = map vertices graphs
        alivess = zipWith liveness retss graphs

    let ads = zipWith4 A.FunDef (zipWith zip bss alivess) retss names argss
        asm = A.program consts ads
    pure $ intercalate "\n" asm ++ "\n"

compile' p = do
    prog <- left (intercalate "\n") $ S.runProgram p
    let Q.Program consts ds = I.program prog
        (retss, names, argss, qss) = unzip4 $ flip map ds $ \(Q.FunDef rets name args qs) -> (rets, name, args, qs)
        graphs = map mkGraph qss
        bss = map vertices graphs
        alivess = zipWith liveness retss graphs
        defss = map reaching graphs

    let bss' = zipWith (\g defs -> eliminate $ Graph (zip (vertices g) defs) (edges g)) graphs defss
        graphs' = zipWith (\g bs -> Graph bs (edges g)) graphs bss'
        alivess' = zipWith liveness retss graphs'

    let ads = zipWith4 A.FunDef (zipWith zip bss' alivess') retss names argss
        asm = A.program consts ads
    pure $ intercalate "\n" asm ++ "\n"

runCode :: String -> IO String
runCode code = withSystemTempDirectory "quickcheck_latc" $ \dir -> do
    let execPath = dir </> "prog"
        objPath = execPath <.> "o"
        asmPath = execPath <.> "s"
    writeFile asmPath code
    c <- runAs asmPath objPath
    when (c /= ExitSuccess) $
        fail "assembly failed"
    c <- runLd objPath execPath
    when (c /= ExitSuccess) $
        fail "linking failed"
    readProcess execPath [] ""

runAs :: FilePath -> FilePath -> IO ExitCode
runAs inp out = rawSystem "as" ["--32", inp, "-o", out]

runLd :: FilePath -> FilePath -> IO ExitCode
runLd inp out = rawSystem "ld" (["-o", out, "-melf_i386", inp] ++ libs)
  where
    libs = map ("lib/" ++) ["runtime.o", "crt1.o", "crti.o", "crtn.o", "libc.a", "libgcc.a", "libgcc_eh.a", "libc.a"]

prop_CalcExpr :: Expr -> Property
prop_CalcExpr e =
    collect (depth e) $
    forAll (genVals $ vars e) $ \vs ->
    not (hasDivideByZero e vs) && depth e > 1 ==>
    let p = simpleProgram e vs in counterexample (pProg p) $
    let expected = eval e vs in counterexample ("expected: " ++ show expected) $
    monadicIO $ do
        --traceM $ "testing expr: " ++ show e
        code <- case compile p of
            Left err -> fail err
            Right c -> pure c
        --traceM $ "asm:\n" ++ code
        out <- run $ runCode code
        let out' = readMaybe out
        assert $ out' == Just expected

        code <- case compile' $ simpleProgram e vs of
            Left err -> fail err
            Right c -> pure c
        out <- run $ runCode code
        let out'' = readMaybe out
        assert $ out' == out''

at :: Eq a => [(a, b)] -> a -> b
at l i = fromJust $ lookup i l
