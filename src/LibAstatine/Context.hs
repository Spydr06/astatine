module LibAstatine.Context (
    InputFile(..),
    OutputFile(..),
    Context(..),
    filenameToOutputFile,
    getContext,
    inputFilePath
) where

import Data.List
import System.IO

newtype InputFile = InputFile String
    deriving Show

inputFilePath :: InputFile -> String
inputFilePath (InputFile s) = s

data OutputFile = Executable String
    | SharedLibrary String
    | ObjectFile String
    | AssemblyFile String
    | RunInPlace [String] -- cmdline arguments
    deriving Show

defaultExecName :: String
defaultExecName = "a.out"

defaultRuntimeFile :: String
defaultRuntimeFile = "runtime.o"

filenameToOutputFile :: String -> OutputFile
filenameToOutputFile name | ".so" `isInfixOf` name = SharedLibrary name
                          | ".o" `isInfixOf` name || 
                            ".a" `isInfixOf` name = ObjectFile name
                          | otherwise = Executable name

data Context = Context {
    progName :: String,
    inputFile :: InputFile,
    outputFile :: OutputFile,
    runtimeFile :: String,

    execute :: Bool,
    silent :: Bool,
    verbose :: Bool,
    
    driverOpts :: [String],

    stdOut :: Handle
} deriving Show

defaultContext :: Context
defaultContext = Context "" (InputFile "") (RunInPlace []) defaultRuntimeFile False False False ["-rdynamic"] stdout

getContext :: String -> Context
getContext progName' = defaultContext { progName = progName' }  

