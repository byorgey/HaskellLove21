-- url: https://open.kattis.com/problems/lastfactorialdigit

import           Control.Arrow ((>>>))

main = interact $
  lines >>> drop 1 >>> map (read >>> solve >>> show) >>> unlines

solve :: Integer -> Integer
solve n = product [1 .. n] `mod` 10
