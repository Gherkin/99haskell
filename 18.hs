slice :: [a] -> Int -> Int -> [a]
slice xs n k = slice' [] xs n k

slice' :: [a] -> [a] -> Int -> Int -> [a]

slice' xs [] _ _ = xs
slice' xs _ 1 1 = xs
slice' xs (y:ys) 1 k = slice' (xs ++ [y]) ys 0 (k - 1)
slice' [] ys n k =  slice' [] (tail ys) (n - 1) (k - 1)
