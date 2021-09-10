-- url: https://open.kattis.com/problems/autori

import Control.Arrow((>>>))

main = interact $ words >>> head >>> solve

solve = map hyphenToSpace >>> words >>> map head
  where
    hyphenToSpace '-' = ' '
    hyphenToSpace c = c

