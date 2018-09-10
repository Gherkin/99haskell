rotate :: [a] -> Int -> [a]
rotate xs n = rotate' xs [] n

rotate' :: [a] -> [a] -> Int -> [a]
rotate' xs ys 0 = xs ++ ys
rotate' [] ys n = rotate' ys [] n
rotate' (x:xs) ys n = rotate' xs (ys ++ [x]) (n - 1)

myReverse :: [a] -> [a]
myReverse (x:[]) = [x]
myReverse (x:xs) = myReverse xs ++ [x]
