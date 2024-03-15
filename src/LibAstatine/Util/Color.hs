module LibAstatine.Util.Color (
    reset,
    bold,
    beginBold,
    endBold,
    red,
    magenta,
    grey,
    yellow,
    cyan,
    errorC,
    status,
    Color(..),
    colorString,
    colorPartially
) where

data Color = Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | BrightBlack
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite

ansiCode :: Color -> String
ansiCode Black = "30"
ansiCode Red = "31"
ansiCode Green = "32"
ansiCode Yellow = "33"
ansiCode Blue = "34"
ansiCode Magenta = "35"
ansiCode Cyan = "36"
ansiCode White = "37"
ansiCode BrightBlack = "90"
ansiCode BrightRed = "91"
ansiCode BrightGreen = "92"
ansiCode BrightYellow = "93"
ansiCode BrightBlue = "94"
ansiCode BrightMagenta = "95"
ansiCode BrightCyan = "96"
ansiCode BrightWhite = "97"

reset :: String
reset = "\ESC[0m"

colorString :: Color -> String -> String
colorString c s = "\ESC[" ++ ansiCode c ++ "m" ++ s ++ reset

colorPartially :: Color -> Bool -> String -> Int -> Int -> String
colorPartially c isBold s lo hi = take lo s ++ (if isBold then beginBold else "") ++ colorString c (slice lo hi) ++ drop hi s
    where slice from to = take (to - from) $ drop from s

beginBold :: String 
beginBold = "\ESC[1m"

endBold :: String
endBold = "\ESC[22m"

bold :: String -> String
bold s = beginBold ++ s ++ reset

red :: String -> String
red = colorString Red

magenta :: String -> String
magenta = colorString Magenta

yellow :: String -> String
yellow = colorString Yellow

grey :: String -> String
grey = colorString BrightBlack

cyan :: String -> String
cyan = colorString Cyan

errorC :: String -> String
errorC = bold . red

status :: String -> String
status = bold . magenta

