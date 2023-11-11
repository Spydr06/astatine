module LibAstatine.Compiler (runCompiler) where

import LibAstatine.Context
import LibAstatine.Util.Log
import LibAstatine.Error
import LibAstatine.File

import System.IO
import LibAstatine.Lexer

runCompiler :: Context -> IO [CompilerError]
runCompiler ctx = do
    logCompilingStatus ctx 
    res <- readSourceFile ctx
    case res of
        Ok source -> case lexFile ctx source of
            Ok tokens -> print tokens >> return []
            Err err -> return [err]
        Err err -> return [err]

