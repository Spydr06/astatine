module LibAstatine.Util.Log (
    logLn,
    logCompilingStatus
) where

import LibAstatine.Context
import LibAstatine.Util.Color

import System.IO

logLn :: Context -> String -> IO ()
logLn ctx ln = if silent ctx then return () else hPutStrLn (stdOut ctx) ln

logCompilingStatus :: Context -> IO ()
logCompilingStatus ctx = let (InputFile filename) = inputFile ctx in
    logLn ctx $ status "Compiling: " ++ filename

