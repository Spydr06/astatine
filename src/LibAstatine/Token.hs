module LibAstatine.Token (
    Kind(..),
    Token(..),
) where

import Data.Int (Int64)
import LibAstatine.Util.Position

data Kind = Operator String
    | Identifier String
    | Keyword String
    | Integer Int64
    | FloatLiteral String
    | StringLiteral String
    | CharLiteral Char
    deriving (Show)

data Token = Token {
    kind :: Kind,
    position :: Position,
    span :: Span
} deriving (Show)


