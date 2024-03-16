module LibAstatine.Compiler (runCompiler) where

import LibAstatine.Context
import LibAstatine.Util.Log
import LibAstatine.Error
import LibAstatine.File

import LibAstatine.Lexer
import qualified LibAstatine.SExpr as SExpr
import qualified LibAstatine.AST as AST

runCompiler :: Context -> IO [CompilerError]
runCompiler ctx = do
    logCompilingStatus ctx 
    res <- readSourceFile ctx
    case res >>= lexFile >>= SExpr.parseAll >>= AST.parseModule of
        Ok ast -> print ast >> return []
        Err err -> return [err] 

