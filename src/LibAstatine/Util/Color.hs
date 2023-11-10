module LibAstatine.Util.Color (
    reset,
    bold,
    red,
    magenta,
    errorC,
    status,
) where

reset :: String
reset = "\ESC[0m"

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ reset

red :: String -> String
red s = "\ESC[31m" ++ s ++ reset

magenta :: String -> String
magenta s = "\ESC[35m" ++ s ++ reset

errorC :: String -> String
errorC = bold . red

status :: String -> String
status = bold . magenta
