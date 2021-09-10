module Main where

import Control.Arrow ((>>>))
import Data.Char

main :: IO ()
main = interact solve

solve :: String -> String
solve = filter isAsciiUpper
