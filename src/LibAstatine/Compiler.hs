module LibAstatine.Compiler (
    CompilerError(..),
    runCompiler
) where

import LibAstatine.Context
import LibAstatine.Util.Color (status)
import LibAstatine.Util.Log

data CompilerError = Foo ()
    deriving (Show)

logCompilingStatus :: Context -> IO ()
logCompilingStatus ctx = let (InputFile filename) = inputFile ctx in
    logLn ctx $ status "Compiling: " ++ filename

runCompiler :: Context -> IO [CompilerError]
runCompiler ctx = do
    logCompilingStatus ctx 
    return []

