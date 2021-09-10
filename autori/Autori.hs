-- url: https://open.kattis.com/problems/autori
main = interact go

go [] = []
go (c:cs) = c:go' cs

go' [] = []
go' ('-':c:cs) = c:go' cs
go' (c:cs) = go' cs

