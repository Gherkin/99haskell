repli :: [a] -> Int -> [a]
repli _ n | n < 0 = []
repli [] _ = []
repli (x:[]) n = multiplyElem x n
repli (x:xs) 1 = x:xs
repli (x:xs) n = multiplyElem x n ++ repli xs n

multiplyElem :: a -> Int -> [a]
multiplyElem _ n | n < 0 = []
multiplyElem x 1 = [x]
multiplyElem x n = x:multiplyElem x (n - 1)
