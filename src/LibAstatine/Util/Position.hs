module LibAstatine.Util.Position (
    Position(..),
    Span(..)
) where

import LibAstatine.Context

data Position = Position {
    filename :: String,
    line :: Int,
    column :: Int
}

instance Show Position where
    show p = filename p ++ ":" ++ show (succ $ line p) ++ ":" ++ show (succ $ column p)

data Span = Span {
    numLines :: Int,
    numColumns :: Int
} deriving (Show)


