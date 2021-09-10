-- url: https://open.kattis.com/problems/autori


main = interact abbrev

abbrev :: String -> String
abbrev (c:cs) = c : go cs
  where
    go ('-':c:cs) = c : go cs
    go (c:cs) = go cs
    go _ = []
