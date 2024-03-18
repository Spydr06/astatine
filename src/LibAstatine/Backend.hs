module LibAstatine.Backend (
    generate    
) where

import LibAstatine.Error (Result (..))
import LibAstatine.Context

import qualified LibAstatine.IR as IR;

import qualified GccJit
import GccJit.Utils (release)

import Control.Monad
import Foreign.C (CString, newCString)
import Foreign (FunPtr, Ptr, nullPtr)
import Foreign.Marshal.Array
import System.Environment (getEnvironment)
import System.Exit (die)
import System.IO

type MainFunction = Int -> Ptr CString -> Ptr CString -> IO Int
foreign import ccall "dynamic" mkFunc :: FunPtr MainFunction -> MainFunction

unwrapOrDie :: IO (Maybe a) -> String -> IO a
unwrapOrDie x msg = do
    x' <- x
    case x' of
        Nothing -> die msg
        Just x'' -> return x''

generate :: Context -> [IR.Decl] -> IO (Result ())
generate ctx mod = do
    jit <- unwrapOrDie GccJit.contextAcquire "Could not acquire libgccjit context" 
    GccJit.setBoolOption jit GccJit.DumpGeneratedCode $ verbose ctx

    print mod

    GccJit.contextAddDriverOption jit $ runtimeFile ctx
    case outputFile ctx of
        Executable path -> GccJit.contextCompileToFile jit GccJit.Executable path
        SharedLibrary path -> GccJit.contextCompileToFile jit GccJit.DynamicLibrary path
        ObjectFile path -> GccJit.contextCompileToFile jit GccJit.ObjectFile path
        AssemblyFile path -> GccJit.contextCompileToFile jit GccJit.Assembler path
        RunInPlace args -> do
            result <- unwrapOrDie (GccJit.contextCompile jit) "Could not compile"
            main <- unwrapOrDie (GccJit.resultGetCode result "main") "No `main` function found."

            let argv = inputFilePath (inputFile ctx) : args
            let argc = length argv
            
            argv' <- toCArgs argv
            envp <- getEnvironment >>= \env -> toCArgs [key ++ "=" ++ value | (key, value) <- env]
            exitCode <- mkFunc main argc argv' envp
            unless (silent ctx) $ do
                putStrLn $ '[' : head argv ++ " exited with code " ++ show exitCode ++ "]"

            hFlush stdout
            release result

    release jit
    return $ Ok ()

toCArgs :: [String] -> IO (Ptr CString)
toCArgs args = do
    cstrs <- mapM newCString args
    newArray0 nullPtr (cstrs ++ [nullPtr])

