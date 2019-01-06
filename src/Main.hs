module Main where

import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Control.Monad
import Data.Monoid
import Data.List
import qualified Data.Set as S
import qualified Data.Map as M

import Semantic.Program
import qualified Intermediate.Generate as I
import Intermediate.Flow
import Intermediate.Liveness
import qualified Quad as Q
import PrintQuad
import qualified Asm.Generate as A
import Frontend
import Intermediate.CSE

import Intermediate.Reaching

printIntermediateFun :: [Defs] -> [S.Set Q.Var] -> String -> [Q.Var] -> [Block] -> String
printIntermediateFun reachingBegins aliveEnds name args blocks = intercalate "\n" $
    [ "fun " <> name <> "(" <> pVars args <> "):"
    , intercalate "\n" $ zipWith3 (\ds as b -> intercalate "\n" $
        [ "{"
        --, "{: " <> printDefs ds
        , pQs b
        , "}: " <> pVars (S.toList as)
        ]) reachingBegins aliveEnds blocks
    ]
  where
    printDefs :: Defs -> String
    printDefs = intercalate ", " . map (\(k, v) -> k <> " <- " <> show v) . M.assocs

main :: IO ()
main = do
    args <- getArgs
    (code, filePath) <- case args of
        inFile:_ -> (,) <$> readFile inFile <*> pure inFile
        _        -> putStrLn "Usage: latc_x86 <file>" *> exitFailure
    prog <- frontend code
    let Q.Program consts ds = I.program prog
        (retss, names, argss, qss) = unzip4 $ flip map ds $ \(Q.FunDef rets name args qs) -> (rets, name, args, qs)
        graphs = map mkGraph qss
        bss = map vertices graphs
        alivess = zipWith liveness retss graphs
        defss = map reaching graphs
        preCSE = intercalate "\n\n" $ zipWith5 printIntermediateFun defss alivess names argss bss

    let bss' = zipWith (\g defs -> eliminate $ Graph (zip (vertices g) defs) (edges g)) graphs defss
        graphs' = zipWith (\g bs -> Graph bs (edges g)) graphs bss'
        alivess' = zipWith liveness retss graphs'
        postCSE = intercalate "\n\n" $ zipWith5 printIntermediateFun defss alivess' names argss bss'

    let ads = zipWith4 A.FunDef (zipWith zip bss' alivess') retss names argss
    --let ads = zipWith4 A.FunDef (zipWith zip bss alivess) retss names argss
        asm = A.program consts ads
    --forM_ asm putStrLn

    let execFilePath = dropExtension filePath
        intPreCSEPath = execFilePath <> "_preCSE.q"
        intPostCSEPath = execFilePath <> "_postCSE.q"
        objFilePath = execFilePath <> ".o"
        asmFilePath = execFilePath <> ".s"
    writeFile intPreCSEPath preCSE
    writeFile intPostCSEPath postCSE
    writeFile asmFilePath $ intercalate "\n" asm <> "\n"
    runAs asmFilePath objFilePath
    runLd objFilePath execFilePath

runAs :: FilePath -> FilePath -> IO ()
runAs inp out = rawSystem "as" ["--32", inp, "-o", out] *> pure ()

runLd :: FilePath -> FilePath -> IO ()
runLd inp out = rawSystem "ld" (["-o", out, "-melf_i386", inp] <> libs) *> pure ()
  where
    libs = map ("lib/" <>) ["runtime.o", "crt1.o", "crti.o", "crtn.o", "libc.a", "libgcc.a", "libgcc_eh.a", "libc.a"]
