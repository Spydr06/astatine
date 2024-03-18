module LibAstatine.Compiler (runCompiler) where

import LibAstatine.Context
import LibAstatine.Util.Log
import LibAstatine.Error
import LibAstatine.File

import LibAstatine.Lexer
import qualified LibAstatine.SExpr as SExpr
import qualified LibAstatine.AST as AST
import qualified LibAstatine.Backend as Backend
import qualified LibAstatine.IR as IR

runCompiler :: Context -> IO [CompilerError]
runCompiler ctx = do
    logCompilingStatus ctx 
    res <- readSourceFile ctx
    case IR.normalize <$> (res >>= lexFile >>= SExpr.parseAll >>= AST.parseModule >>= AST.checkModule) of
        Ok ir -> do 
            res <- Backend.generate ctx ir
            print res
            return []
        Err err -> return [err] 

