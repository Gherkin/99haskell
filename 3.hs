elementAt :: [a] -> Int -> a
elementAt xs n = if n == 1
                 then head xs
                 else elementAt (tail xs) (n - 1)
