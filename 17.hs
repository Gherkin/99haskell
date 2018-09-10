split :: [a] -> Int -> ([a], [a])
split xs n = split' [] xs n

split' :: [a] -> [a] -> Int -> ([a], [a])
split' xs [] _ = (xs, [])
split' xs ys 0 = (xs, ys)
split' xs (y:ys) n = split' (xs ++ [y]) ys (n - 1)

