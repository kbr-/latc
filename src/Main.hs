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

import PrintQuad
import Frontend
import Semantic.Program
import Intermediate.Flow
import Intermediate.Liveness
import qualified Quad as Q
import qualified Intermediate.Generate as I
import qualified Asm.Generate as A

printIntermediateFun :: [S.Set Q.Var] -> String -> [Q.Var] -> [Block] -> String
printIntermediateFun aliveEnds name args blocks = intercalate "\n" $
    [ "fun " <> name <> "(" <> pVars args <> "):"
    , intercalate "\n" $ zipWith (\as b -> intercalate "\n" $
        [ "{"
        , pQs b
        , "}: " <> pVars (S.toList as)
        ]) aliveEnds blocks
    ]

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

        ads = zipWith4 A.FunDef (zipWith zip bss alivess) retss names argss
        quads = intercalate "\n\n" $ zipWith4 printIntermediateFun alivess names argss bss
        asm = intercalate "\n" (A.program consts ads) <> "\n"

    let execFilePath = dropExtension filePath
        intFilePath = execFilePath <> ".q"
        objFilePath = execFilePath <> ".o"
        asmFilePath = execFilePath <> ".s"
    writeFile intFilePath quads
    writeFile asmFilePath asm
    runAs asmFilePath objFilePath
    runLd objFilePath execFilePath

runAs :: FilePath -> FilePath -> IO ()
runAs inp out = rawSystem "as" ["--32", inp, "-o", out] *> pure ()

runLd :: FilePath -> FilePath -> IO ()
runLd inp out = rawSystem "ld" (["-o", out, "-melf_i386", inp] <> libs) *> pure ()
  where
    libs = map ("lib/" <>) ["runtime.o", "crt1.o", "crti.o", "crtn.o", "libc.a", "libgcc.a", "libgcc_eh.a", "libc.a"]
