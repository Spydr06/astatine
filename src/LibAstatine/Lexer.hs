{-# LANGUAGE TupleSections #-}

module LibAstatine.Lexer (
    lexFile
) where

import LibAstatine.File (SourceFile (sourceLines, sourceFilePath))
import LibAstatine.Error
import LibAstatine.Token
import LibAstatine.Util.Position

import Data.Char (isSpace, isAlphaNum, isDigit)

lexFile :: SourceFile -> Result [Positioned Token]
lexFile sourcefile = lexLines (sourceLines sourcefile) $ defaultPosition $ sourceFilePath sourcefile

lexLines :: [String] -> Position -> Result [Positioned Token]
lexLines [] _ = Ok []
lexLines (x:xs) pos = lexAllTokens x pos >>= \tokens -> (tokens++) <$> lexLines xs (succLine pos)

lexAllTokens :: String -> Position -> Result [Positioned Token]
lexAllTokens input pos = case lexToken input pos of
    Err err -> Err err
    Ok (Nothing, _) -> Ok []
    Ok (Just token, ws) -> let len = ws + tokenLength (positioned token) in 
        (token:) <$> lexAllTokens (drop len input) (incColumn len pos)

isIdent :: Char -> Bool
isIdent = (||) <$> isAlphaNum <*> (`elem` "+-*/&|^%$@#?<>,.:;")

lexToken :: String -> Position -> Result (Maybe (Positioned Token), Int)
lexToken input pos = let (s, ws) = skipWhitespace input 0 in fmap (,ws) (case s of
    [] -> Ok Nothing
    (c:cs) -> case lookup c symbols of
        Just token -> Ok $ Just $ Positioned pos token
        Nothing | isDigit c -> Ok $ Just $ Positioned pos $ IntegerLiteral $ c : takeWhile isDigit cs -- TODO: float literal 
                | isIdent c -> Ok $ Just $ Positioned pos $ Identifier $ c : takeWhile isIdent cs
                | c == '"' -> case lexStringLit cs of
                    Just i -> Ok $ Just $ Positioned pos $ StringLiteral $ take i cs
                    Nothing -> Err $ LexerError pos "invalid string literal"
                | c == '\'' -> case cs of
                    ('\\' : c' : '\'' : _) -> Ok $ Just $ Positioned pos $ EscapedCharLiteral c'
                    (c' : '\'' : _) -> Ok $ Just $ Positioned pos $ CharLiteral c'
                    _ -> Err $ LexerError pos "invalid character literal"
        _ -> Err $ LexerError pos $ "unknown token " ++ [c])
    where symbols = [
                ('(', LParen),   (')', RParen),
                ('[', LBracket), (']', RBracket),
                ('{', LBrace),   ('}', RBrace),
                ('#', QuoteChar)
            ]

lexStringLit :: String -> Maybe Int
lexStringLit [] = Nothing
lexStringLit ['\\'] = Nothing
lexStringLit ('\\':_:cs) = (2+) <$> lexStringLit cs 
lexStringLit ('"':_) = Just 0
lexStringLit (_:cs) = succ <$> lexStringLit cs

skipWhitespace :: String -> Int -> (String, Int)
skipWhitespace [] i = ([], i)
skipWhitespace ('-':'-':_) i = ([], i)
skipWhitespace s@(x:xs) i | isSpace x = skipWhitespace xs $ succ i
                          | otherwise = (s, i)

