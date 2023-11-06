module LibAstatine.Util.Color (
    reset,
    bold,
    red,
    errorC
) where

reset :: String
reset = "\ESC[0m"

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ reset

red :: String -> String
red s = "\ESC[31m" ++ s ++ reset

errorC :: String -> String
errorC = bold . red

