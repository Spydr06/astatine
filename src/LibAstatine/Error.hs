module LibAstatine.Error (
    CompilerError(..),
    Result(..)
) where

import LibAstatine.Util.Color
import LibAstatine.Util.Position
import LibAstatine.Token

data Result a = Ok a
    | Err CompilerError

data CompilerError = ReadFileError String String -- filename error
    | LexerError Token String (Maybe String) (Maybe String) -- token error line hint

instance Show CompilerError where
    show (ReadFileError filename error) = errorC "[Error]" ++ " Could not read `" ++ filename ++ "`: " ++ error
    show (LexerError token error sourceLine hint) = errorC "[Error]" ++ " " ++ show (position token) ++ ": " ++ error ++ ":" ++ case sourceLine of
        Just contents -> "\n" ++ highlightToken token contents hint
        Nothing -> ""

highlightToken :: Token -> String -> Maybe String -> String
highlightToken t s hint = let lineNumStr = justifyLeft 4 ' ' $ show $ succ $ line $ position t in
    grey (' ' : beginBold ++ lineNumStr ++ endBold ++ " | ") ++ colorPartially BrightYellow True s (column $ position t) (numColumns $ LibAstatine.Token.span t) ++ "\n" ++ 
    grey (replicate (2 + length lineNumStr) ' ' ++ "| ") ++ replicate (column $ position t) ' ' ++ yellow (replicate (numColumns $ LibAstatine.Token.span t) '~') ++ getHint
        where getHint = case hint of
                Just hint -> grey $ " <- " ++ beginBold ++ "hint: " ++ endBold ++ hint
                Nothing -> ""


justifyLeft, justifyRight :: Int -> a -> [a] -> [a]
justifyLeft  n c s = s ++ replicate (n - length s) c
justifyRight n c s = replicate (n - length s) c ++ s

