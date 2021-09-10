import           Data.List.Split
main=interact$solve.drop 1 . map read . words
solve :: [Int]->String
solve ts
  |odd(length ts)="still runnin"
  |otherwise=show.sum.map(\[a,b]->b-a).chunksOf 2$ts
