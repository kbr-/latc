module Compile where

import Control.Arrow
import Data.List
import Data.Monoid
import qualified Data.Set as S
import System.Exit
import System.IO
import System.Process

import qualified AbsLatte as T
import ParLatte
import ErrM

import PrintQuad
import Semantic.Program
import Intermediate.Flow
import Intermediate.Liveness
import Intermediate.Reaching
import Intermediate.CSE
import qualified Annotated as A
import qualified Asm.Generate as Asm
import qualified Intermediate.Fold as F
import qualified Intermediate.Copy as C
import qualified Intermediate.Dead as D
import qualified Intermediate.Peephole as P
import qualified Quad as Q
import qualified Intermediate.Generate as I

type Pos = Maybe (Int, Int)

type Opt = Graph Block -> Bool -> [Defs] -> (Graph Block, [Defs])

fold :: Opt
fold g _ ds = (g', reaching g')
  where
    g' = Graph bs $ mkEdges bs
    bs = F.fold g ds

cse :: Opt
cse g _ ds = (Graph (eliminate g ds) $ edges g, ds)

copy :: Opt
copy g _ ds = (Graph (C.copy g ds) $ edges g, ds)

cleanDead :: Opt
cleanDead g rets _ = (g', reaching g')
  where
    g' = Graph (D.eliminate rets g) $ edges g

peephole :: Opt
peephole g _ _ = (g', reaching g')
  where
    g' = mkGraph $ P.peephole $ concat $ vertices g

opts :: Int -> [Opt]
opts n = concatMap (replicate n) [fold, cse, copy, cleanDead, peephole]

flow :: [Opt] -> Q.TopDef -> Asm.TopDef
flow opts (Q.FunDef rets name args qs) = Asm.FunDef (zip bs alives) rets name args
  where
    bs = vertices graph'
    alives = liveness rets graph'
    graph' = fst $ foldl (\(g, ds) opt -> opt g rets ds) (graph, reaching graph) opts
    graph = mkGraph qs

parse :: String -> Either String (T.Program Pos)
parse code = case pProgram $ myLexer code of
    Bad s   -> Left s
    Ok tree -> pure tree

semantic :: T.Program Pos -> Either String A.Program
semantic prog = left (intercalate "\n") $ runProgram prog

frontend :: String -> IO A.Program
frontend code = case parse code >>= semantic of
    Left err -> hPutStrLn stderr ("ERROR\n" ++ err) *> exitFailure
    Right x  -> hPutStrLn stderr "OK" *> pure x

generate :: A.Program -> [Opt] -> (String, String)
generate prog opts =
    (intercalate "\n\n" $ map printDef atds,
     intercalate "\n" (Asm.program consts atds) <> "\n")
  where
    atds = map (flow opts) tds
    Q.Program consts tds = I.program prog

    printDef :: Asm.TopDef -> String
    printDef (Asm.FunDef blocks rets name args) = intercalate "\n" $
        [ "fun " <> name <> "(" <> pVars args <> "):"
        , intercalate "\n" $ map (\(b, as) -> intercalate "\n" $
            [ "{"
            , pQs b
            , "}: " <> pVars (S.toList as)
            ]) blocks
        ]

runAs :: FilePath -> FilePath -> IO ()
runAs inp out = callProcess "as" ["--32", inp, "-o", out]

runLd :: FilePath -> FilePath -> IO ()
runLd inp out = callProcess "ld" (["-o", out, "-melf_i386", inp] ++ libs)
  where
    libs = map ("lib/" ++) ["runtime.o", "crt1.o", "crti.o", "crtn.o", "libc.a"]
