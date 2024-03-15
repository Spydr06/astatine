module LibAstatine.Util.Position (
    Position(..),
    Positioned(..),
    defaultPosition,
    defaultPositioned,
    incLine,
    succLine,
    incColumn,
    succColumn,
) where

data Position = Position {
    filename :: String,
    line :: Int,
    column :: Int
}

instance Show Position where
    show p = filename p ++ ":" ++ show (succ $ line p) ++ ":" ++ show (succ $ column p)

defaultPosition :: String -> Position
defaultPosition filename = Position filename 0 0

incLine :: Int -> Position -> Position
incLine i p = p { line = line p + i }

succLine :: Position -> Position
succLine = incLine 1

incColumn :: Int -> Position -> Position
incColumn i p = p { column = column p + i }

succColumn :: Position -> Position
succColumn = incColumn 1

data Positioned a = Positioned {
    position :: Position,
    positioned :: a
}

defaultPositioned :: String -> a -> Positioned a
defaultPositioned = Positioned . defaultPosition

instance (Show a) => Show (Positioned a) where
    show p = show (position p) ++ ": " ++ show (positioned p)

instance Functor Positioned where
    fmap f p = p { positioned = f $ positioned p }

instance Applicative Positioned where
    pure = defaultPositioned ""
    (<*>) = fmap . positioned

instance Monad Positioned where
    a >>= f = f $ positioned a


