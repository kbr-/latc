module Frontend where

import System.Exit
import Data.List

import AbsLatte
import ParLatte
import ErrM

import Semantic.Program
import qualified Annotated as T

frontend :: String -> IO T.Program
frontend code = do
    maybeProg <- case pProgram $ myLexer code of
        Bad s   -> putStrLn ("ERROR\n" ++ s) *> exitFailure
        Ok tree -> pure $ runProgram tree
    case maybeProg of
        Left errs -> putStrLn "ERROR" *> putStrLn (intercalate "\n" errs) *> exitFailure
        Right x -> putStrLn "OK" *> pure x
