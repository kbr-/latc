{-# LANGUAGE RecordWildCards #-}
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
import Intermediate.Reaching
import Intermediate.CSE
import qualified Quad as Q
import qualified Intermediate.Generate as I
import qualified Asm.Generate as A

printDef :: A.TopDef -> String
printDef (A.FunDef blocks rets name args) = intercalate "\n" $
    [ "fun " <> name <> "(" <> pVars args <> "):"
    , intercalate "\n" $ map (\(b, as) -> intercalate "\n" $
        [ "{"
        , pQs b
        , "}: " <> pVars (S.toList as)
        ]) blocks
    ]

flow :: Q.TopDef -> A.TopDef
flow (Q.FunDef rets name args qs) = A.FunDef (zip bs alives) rets name args
  where
    bs = vertices graph
    alives = liveness rets graph
    graph = mkGraph qs

cse :: Graph Block -> Graph Block
cse g = Graph (eliminate $ Graph (zip (vertices g) (reaching g)) (edges g)) (edges g)

main :: IO ()
main = do
    args <- getArgs
    (code, filePath) <- case args of
        inFile:_ -> (,) <$> readFile inFile <*> pure inFile
        _        -> putStrLn "Usage: latc_x86 <file>" *> exitFailure
    prog <- frontend code

    let Q.Program consts ds = I.program prog
        ads = map flow ds
        quads = intercalate "\n\n" $ map printDef ads
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
    libs = map ("lib/" <>) ["runtime.o", "crt1.o", "crti.o", "crtn.o", "libc.a"]
