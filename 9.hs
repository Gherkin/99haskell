pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:[]) = [[x]]
pack (x:xs) = pack' [x] xs

pack' :: Eq a => [a] -> [a] -> [[a]]
pack' (x:xs) (y:[]) = if x == y then
                          [y:x:xs]
                      else
                          [x:xs, [y]]
pack' (x:xs) (y:ys) = if x == y then
                          pack' (y:x:xs) (ys)
                      else
                          [x:xs] ++ pack' [y] ys
