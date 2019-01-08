module Frontend where

import System.Exit
import System.IO
import Data.List

import AbsLatte
import ParLatte
import ErrM

import Semantic.Program
import qualified Annotated as T

frontend :: String -> IO T.Program
frontend code = do
    maybeProg <- case pProgram $ myLexer code of
        Bad s   -> hPutStrLn stderr ("ERROR\n" ++ s) *> exitFailure
        Ok tree -> pure $ runProgram tree
    case maybeProg of
        Left errs -> hPutStrLn stderr ("ERROR\n" ++ intercalate "\n" errs) *> exitFailure
        Right x -> hPutStrLn stderr "OK" *> pure x
