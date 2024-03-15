module LibAstatine.Compiler (runCompiler) where

import LibAstatine.Context
import LibAstatine.Util.Log
import LibAstatine.Error
import LibAstatine.File

import LibAstatine.Lexer
import qualified LibAstatine.SExpr as SExpr

runCompiler :: Context -> IO [CompilerError]
runCompiler ctx = do
    logCompilingStatus ctx 
    res <- readSourceFile ctx
    case res >>= lexFile >>= SExpr.parseAll of
        Ok expr -> putStrLn (unlines $ map show expr) >> return []
        Err err -> return [err] 

