module Main where

import System.Exit
import System.Environment

import Compile

main :: IO ()
main = do
    args <- getArgs
    (code, filePath) <- case args of
        inFile:_ -> (,) <$> readFile inFile <*> pure inFile
        _        -> putStrLn "Usage: latc <file>" *> exitFailure
    _ <- frontend code
    pure ()
