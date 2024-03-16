module LibAstatine.Error (
    CompilerError(..),
    Result(..)
) where

import LibAstatine.Util.Color
import LibAstatine.Util.Position
import LibAstatine.Token

data Result a = Ok a
    | Err CompilerError

instance Functor Result where
    fmap f r = case r of
        Ok ok -> Ok $ f ok
        Err err -> Err err

instance Applicative Result where
    pure = Ok
    f <*> r = case f of
        Ok ok -> fmap ok r
        Err err -> Err err
    
instance Monad Result where
    a >>= f = case a of
        Ok ok -> f ok
        Err err -> Err err

data CompilerError = ReadFileError String String -- filename error
    | LexerError Position String
    | ParseError (Positioned Token) String
    | UnexpectedEof String
    | UnexpectedSExpr String

{- instance Show CompilerError where
    show (ReadFileError filename error) = errorC "[Error]" ++ " Could not read `" ++ filename ++ "`: " ++ error
    show (LexerError token error sourceLine hint) = errorC "[Error]" ++ " " ++ show (position token) ++ ": " ++ error ++ ":" ++ case sourceLine of
        Just contents -> "\n" ++ highlightToken token contents hint
        Nothing -> "" -}

{- highlightToken :: Token -> String -> Maybe String -> String
highlightToken t s hint = let lineNumStr = justifyRight 4 ' ' $ show $ succ $ line $ position t in
    grey (' ' : beginBold ++ lineNumStr ++ endBold ++ " | ") ++ colorPartially BrightYellow True s (column $ position t) (numColumns $ LibAstatine.Token.span t) ++ "\n" ++ 
    grey (replicate (2 + length lineNumStr) ' ' ++ "| ") ++ replicate (column $ position t) ' ' ++ yellow (replicate (numColumns $ LibAstatine.Token.span t) '~') ++ getHint
        where getHint = case hint of
                Just hint -> grey $ " <- " ++ beginBold ++ "hint: " ++ endBold ++ hint
                Nothing -> "" -}

instance Show CompilerError where
    show (ReadFileError filename msg) = errorC "[Error]" ++ " Could not read `" ++ filename ++ "`: " ++ msg
    show (LexerError position msg) = errorC "[Error] " ++ show position ++ ": " ++ msg
    show (ParseError position msg) = errorC "[Error] " ++ show position ++ ": " ++ msg
    show (UnexpectedEof msg) = errorC "[Error] " ++ "Unexpected end of file: " ++ msg
    show (UnexpectedSExpr msg) = errorC "[Error] " ++ "Unexpected s-expression: " ++ msg
