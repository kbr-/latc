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
import Intermediate.Generate
import Intermediate.Flow
import Intermediate.Liveness
import qualified Quad as Q
import PrintQuad
import Asm.Generate

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
    forM_ prog $ \f -> do
        let fd@(Q.FunDef rets name args qs) = generate f
            cfg = mkGraph qs
            bs = liveness rets cfg
        putStrLn $ "fun " <> name <> "(" <> pVars args <> "):\n"
        forM_ bs $ \(b, l) -> do
            putStrLn "{"
            putStrLn $ pQs b
            putStrLn $ "}, live at end: " <> pVars (S.toList l)
        putStrLn "asm:"
        mapM_ putStrLn $ fun bs rets args name
