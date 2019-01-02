module Main where

import System.Environment
import System.Exit
import System.FilePath
import System.Process
import Control.Monad
import Data.Monoid
import Data.List
import qualified Data.Set as S

import Semantic.Program
import qualified Intermediate.Generate as I
import Intermediate.Flow
import Intermediate.Liveness
import qualified Quad as Q
import PrintQuad
import qualified Asm.Generate as A
import Frontend

main :: IO ()
main = do
    args <- getArgs
    (code, filePath) <- case args of
        inFile:_ -> (,) <$> readFile inFile <*> pure inFile
        _        -> putStrLn "Usage: latc_x86 <file>" *> exitFailure
    prog <- frontend code
    let Q.Program consts ds = I.program prog
        ds' = flip map ds $ \(Q.FunDef rets name args qs) ->
            A.FunDef (liveness rets $ mkGraph qs) rets name args
    forM_ ds' $ \(A.FunDef bs rets name args) -> do
        putStrLn $ "\nfun " <> name <> "(" <> pVars args <> "):"
        forM_ bs $ \(b, l) -> do
            putStrLn "{"
            putStrLn $ pQs b
            putStrLn $ "}: " <> pVars (S.toList l)
    let execFilePath = dropExtension filePath
        objFilePath = execFilePath <> ".o"
        asmFilePath = execFilePath <> ".s"
    writeFile asmFilePath $ intercalate "\n" (A.program consts ds') <> "\n"
    runAs asmFilePath objFilePath
    runLd objFilePath execFilePath

runAs :: FilePath -> FilePath -> IO ()
runAs inp out = rawSystem "as" ["--32", inp, "-o", out] *> pure ()

runLd :: FilePath -> FilePath -> IO ()
runLd inp out = rawSystem "ld" (["-o", out, "-melf_i386", inp] <> libs) *> pure ()
  where
    libs = map ("lib/" <>) ["runtime.o", "crt1.o", "crti.o", "crtn.o", "libc.a"]
