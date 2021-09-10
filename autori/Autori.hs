-- url: https://open.kattis.com/problems/autori

import           Control.Arrow                  ( (>>>) )
import           Data.Char                      ( isUpper )

main :: IO ()
main = interact $ lines >>> map solve >>> unlines

solve :: String -> String
solve [] = []
solve (x : xs) | isUpper x = x : solve xs
               | otherwise = solve xs
