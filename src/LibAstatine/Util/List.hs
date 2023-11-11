module LibAstatine.Util.List (
    justifyLeft,
    justifyRight
) where

justifyLeft, justifyRight :: Int -> a -> [a] -> [a]
justifyLeft  n c s = s ++ replicate (n - length s) c
justifyRight n c s = replicate (n - length s) c ++ s

