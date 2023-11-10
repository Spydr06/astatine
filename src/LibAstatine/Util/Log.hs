module LibAstatine.Util.Log (
    logLn
) where

import LibAstatine.Context

import System.IO

logLn :: Context -> String -> IO ()
logLn ctx ln = if silent ctx then return () else hPutStrLn (stdOut ctx) ln

