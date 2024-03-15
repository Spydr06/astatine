module Args (
    Error,
    parseArgs,
    execFinites,
    applyArgs,
) where

import qualified LibAstatine.Util.Color as Color
import LibAstatine.Context
import LibAstatine.Version
import Data.Maybe (listToMaybe)
import System.Exit (exitSuccess)
import Data.Version (showVersion)
import LibAstatine (copyrightNotice)

data Error = ArgError {
    prog :: String,
    message :: String,
    isFatal :: Bool
}

instance Show Error where
    show e = concat [
            prog e, ": ", Color.errorC "[Error]",
            " ", message e, 
            if isFatal e then "\naborting." else ""
        ]

data Arg = Help
    | Version
    | Run
    | Silent
    | OutputFile String
    | InputFile String
    deriving Show

takesParam :: Arg -> Bool
takesParam (OutputFile _) = True
takesParam _ = False

isFinite :: Arg -> Bool
isFinite Help = True
isFinite Version = True
isFinite _ = False

parseArg :: Context -> [String] -> Either Arg Error 
parseArg ctx (arg:xs) | arg `elem` ["-h", "--help"] = Left Help
                      | arg == "--version" = Left Version
                      | arg `elem` ["-r", "--run"] = Left Run
                      | arg == "--silent" = Left Silent
                      | arg `elem` ["-o", "--output"] = withParam OutputFile
                      | head arg /= '-' = Left $ Args.InputFile arg
                      | otherwise = err $ "Unknown argument `" ++ arg ++ "`. Use `--help` for help."
    where err msg     = Right $ ArgError (progName ctx) msg True
          withParam f = case listToMaybe xs of
              Nothing -> err $ "Argument `" ++ arg ++ "` expects parameter afterwards."
              Just param -> Left $ f param
parseArg _ [] = undefined

parseArgs :: Context -> [String] -> Either [Arg] Error
parseArgs _ [] = Left []
parseArgs ctx args@(_:xs) = case parseArg ctx args of
    Left a -> let remaining = if takesParam a then tail xs else xs in
        case parseArgs ctx remaining of
            Left as -> Left $ a : as
            e -> e 
    Right e -> Right e

helpText :: Context -> String
helpText _ = "foo"

execFinite :: Context -> Arg -> IO ()
execFinite ctx Help = putStrLn $ helpText ctx
execFinite ctx Version = putStrLn $ concat [
        progName ctx, " (The Astatine Compiler) ",
        showVersion version, "\n",
        copyrightNotice
    ]
execFinite _ _ = undefined

execFinites :: Context -> [Arg] -> IO ()
execFinites ctx args = case filter isFinite args of
    (a:_) -> execFinite ctx a >> exitSuccess
    [] -> return ()

applyArg :: Context -> Arg -> Context
applyArg ctx Run = ctx { execute = True }
applyArg ctx Silent = ctx { silent = True }
applyArg ctx (OutputFile f) = ctx { outputFile = filenameToOutputFile f }
applyArg ctx (Args.InputFile f) = ctx { inputFile = LibAstatine.Context.InputFile f }
applyArg _ _ = undefined

applyArgs :: Context -> [Arg] -> Context
applyArgs = foldl applyArg

