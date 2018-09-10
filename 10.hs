encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode (x:[]) = [(1, x)]
encode (x:xs) = encode' (1, x) xs

encode' :: Eq a => (Int, a) -> [a] -> [(Int, a)]
encode' (n, x) (y:[]) = if x == y then
                            [(n + 1, x)]
                        else
                            [(n, x), (1, y)]
encode' (n, x) (y:ys) =
                        if x == y then
                            encode' (n + 1, x) ys
                        else
                            [(n, x)] ++ encode' (1, y) ys

