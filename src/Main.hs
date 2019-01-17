module Main where

import System.Environment
import System.Exit
import System.FilePath

import Compile

main :: IO ()
main = do
    args <- getArgs
    (code, filePath) <- case args of
        inFile:_ -> (,) <$> readFile inFile <*> pure inFile
        _        -> putStrLn "Usage: latc_x86 <file>" *> exitFailure
    prog <- frontend code

    let (quads, asm) = generate prog $ opts 3

    let execFilePath = dropExtension filePath
        intFilePath = execFilePath <.> ".q"
        objFilePath = execFilePath <.> ".o"
        asmFilePath = execFilePath <.> ".s"
    writeFile intFilePath quads
    writeFile asmFilePath asm
    runAs asmFilePath objFilePath
    runLd objFilePath execFilePath
