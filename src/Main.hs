module Main where

import System.Environment
import System.Exit
import Data.List

import AbsLatte
import ParLatte
import ErrM

import Semantic.Program
import Intermediate.Generate
import PrintQuad

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
    mapM_ (putStrLn . printFunDef . generate) prog
