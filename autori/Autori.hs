-- url: https://open.kattis.com/problems/autori

import           Control.Arrow ((>>>))

main = interact $
  lines >>> head >>> solve >>> (:[]) >>> unlines

solve :: String -> String
solve [] = []
solve (x:xs) = x : solve (drop 1 $ dropWhile (/= '-') xs)
