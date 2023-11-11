module LibAstatine.File (
    SourceFile(..),
    readSourceFile
) where

import LibAstatine.Error
import LibAstatine.Context

import Data.Functor ((<&>))

import GHC.IO.Exception (IOException(ioe_description))
import Control.Exception (try, handle)

data SourceFile = SourceFile {
    sourceFilePath :: String,
    sourceLines :: [String],
    numLines :: Int
}

fromLines :: String -> [String] -> SourceFile
fromLines filename lines = SourceFile filename lines $ length lines

getLine :: SourceFile -> Int -> Maybe String
getLine file lineNum | lineNum >= numLines file = Nothing
                     | otherwise = Just $ sourceLines file !! lineNum

readSourceFile :: Context -> IO (Result SourceFile)
readSourceFile ctx = handle readHandler $ let (InputFile filename) = inputFile ctx in 
        readFile filename <&> Ok . fromLines filename . Prelude.lines
    where readHandler = let (InputFile filename) = inputFile ctx in 
            return . Err . ReadFileError filename . ioe_description

