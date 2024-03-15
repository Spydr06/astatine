module LibAstatine.Token (
    Token(..),
    tokenLength
) where

data Token = LParen | RParen
    | LBrace | RBrace
    | LBracket | RBracket
    | Identifier String
    | IntegerLiteral String
    | FloatLiteral String
    | StringLiteral String
    | CharLiteral Char
    | EscapedCharLiteral Char
    | QuoteChar
    deriving (Show, Eq)

tokenLength :: Token -> Int
tokenLength t | t `elem` [
        LParen, RParen,
        LBrace, RBrace,
        LBracket, RBracket,
        QuoteChar
    ] = 1
tokenLength (Identifier ident) = length ident
tokenLength (IntegerLiteral lit) = length lit
tokenLength (FloatLiteral lit) = length lit
tokenLength (StringLiteral lit) = length lit + 2
tokenLength (CharLiteral _) = 3
tokenLength (EscapedCharLiteral _) = 4
tokenLength _ = undefined

