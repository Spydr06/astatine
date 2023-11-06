module Main where

import Args
import LibAstatine.Context

import System.Environment
import System.IO
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    ctx <- getContext <$> getProgName
    parsedArgs <- case parseArgs ctx args of 
        Left a -> return a
        Right e -> hPrint stderr e >> exitFailure 
    execFinites ctx parsedArgs
    print parsedArgs
