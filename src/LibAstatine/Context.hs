module LibAstatine.Context (
    InputFile(..),
    OutputFile(..),
    Context(..),
    getContext
) where

newtype InputFile = InputFile String
    deriving Show

data OutputFile = Executable String
    | SharedLibrary String
    | ObjectFile String
    deriving Show

defaultExecName :: String
defaultExecName = "a.out"

data Context = Context {
    progName :: String,
    inputFile :: InputFile,
    outputFile :: OutputFile
} deriving Show

defaultContext :: Context
defaultContext = Context "" (InputFile "") $ Executable defaultExecName

getContext :: String -> Context
getContext progName = defaultContext { progName = progName }  

