module Main where

import System.Environment
import System.Exit
import Control.Monad
import Data.List
import Data.Monoid
import qualified Data.Set as S

import AbsLatte
import ParLatte
import ErrM

import Semantic.Program
import qualified Intermediate.Generate as I
import Intermediate.Flow
import Intermediate.Liveness
import qualified Quad as Q
import PrintQuad
import qualified Asm.Generate as A

main :: IO ()
main = do
    args <- getArgs
    (code, filePath) <- case args of
        inFile:_ -> (,) <$> readFile inFile <*> pure inFile
        _        -> putStrLn "Usage: latc <file>" *> exitFailure
    maybeProg <- case pProgram $ myLexer code of
        Bad s   -> putStrLn ("ERROR\n" ++ s) *> exitFailure
        Ok tree -> pure $ runProgram tree
    prog <- case maybeProg of
        Left errs -> putStrLn "ERROR" *> putStrLn (concat . intersperse "\n" $ errs) *> exitFailure
        Right x -> putStrLn "OK" *> pure x
    let Q.Program consts ds = I.program prog
        ds' = flip map ds $ \(Q.FunDef rets name args qs) ->
            A.FunDef (liveness rets $ mkGraph qs) rets name args
    forM_ ds' $ \(A.FunDef bs rets name args) -> do
        putStrLn $ "\nfun " <> name <> "(" <> pVars args <> "):"
        forM_ bs $ \(b, l) -> do
            putStrLn "{"
            putStrLn $ pQs b
            putStrLn $ "}: " <> pVars (S.toList l)
    putStrLn "\nasm:\n"
    mapM_ putStrLn $ A.program consts ds'
