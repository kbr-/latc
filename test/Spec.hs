{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import System.Process
import System.FilePath
import System.IO.Temp
import System.Exit
import System.Timeout

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

main :: IO ()
main = hspec $
    it "calculates arithmetic expressions" $ property prop_CalcExpr

data Expr
    = Op Expr Op Expr
    | Const Int
    | Var Name

data Op = Plus | Minus | Times | Div | Mod
    deriving Eq

newtype Name = Name String
    deriving (Eq, Show)

instance Arbitrary Op where
    arbitrary = frequency . map (second pure) $ [(6, Plus), (2, Minus), (0, Times), (1, Div), (1, Mod)]

instance Arbitrary Name where
    arbitrary = oneof $ map (pure . Name) ["a", "b", "c", "d", "e"]

instance Arbitrary Expr where
    arbitrary = choose (10, 15) >>= \s -> resize s $ sized expr

instance Show Expr where
    show = \case
        Op a op b    -> "(" ++ show a ++ " " ++ pOp op ++ " " ++ show b ++ ")"
        Var (Name v) -> v
        Const x      -> show x

expr :: Int -> Gen Expr
expr 0 = oneof $ [Const <$> arbitrary, Var <$> arbitrary]
expr n | n > 0 = frequency $
    [ (4, Op    <$> subexpr <*> arbitrary <*> subexpr)
    , (2, Const <$> arbitrary)
    , (1, Var   <$> arbitrary)
    ]
  where
    subexpr = expr (n - 1)

genVals :: [Name] -> Gen [(Name, Int)]
genVals = mapM (\v -> (v, ) <$> arbitrary)

vars :: Expr -> [Name]
vars = \case
    Op a _ b -> vars a `union` vars b
    Const _  -> []
    Var v    -> [v]

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
    Op a op b -> toFun op (eval a vs) (eval b vs)
    Var v     -> vs `at` v
    Const x   -> x
  where
    toFun = \case
        Plus  -> (+)
        Minus -> (-)
        Times -> (*)
        Div   -> quot
        Mod   -> rem

type Pos = Maybe (Int, Int)

toAbsExpr :: Expr -> T.Expr Pos
toAbsExpr = \case
    Op a op b | elem op [Plus, Minus] ->
        T.EAdd pos (toAbsExpr a) (toAbsAddOp op) (toAbsExpr b)
    Op a op b ->
        T.EMul pos (toAbsExpr a) (toAbsMulOp op) (toAbsExpr b)
    Const x -> T.ELitInt pos $ fromIntegral x
    Var (Name v) -> T.EVar pos (T.Ident v)
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

simpleProgram :: Expr -> [(Name, Int)] -> T.Program Pos
simpleProgram e vs = T.Program pos $
    [ T.FnDef pos (T.Int pos) (T.Ident "main") [] $ T.Block pos $
        map (uncurry define) vs ++
        [ T.SExp pos $ T.EApp pos (T.Ident "printInt") [toAbsExpr e]
        , T.Ret pos $ T.ELitInt pos 0
        ]
    ]
  where
    define :: Name -> Int -> T.Stmt Pos
    define (Name v) x = T.Decl pos (T.Int pos) [T.Init pos (T.Ident v) $ T.ELitInt pos $ fromIntegral x]

compile :: T.Program Pos -> Either String String
compile p = do
    prog <- left (intercalate "\n") $ S.runProgram p
    let Q.Program consts ds = I.program prog
        ds' = flip map ds $ \(Q.FunDef rets name args qs) ->
            A.FunDef (liveness rets $ mkGraph qs) rets name args
        asm = A.program consts ds'
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
    monadicIO $ do
        code <- case compile $ simpleProgram e vs of
            Left err -> fail err
            Right c -> pure c
        out <- run $ runCode code
        let out' = readMaybe out
            ev = eval e vs
        assert $ out' == Just ev

at :: Eq a => [(a, b)] -> a -> b
at l i = fromJust $ lookup i l
