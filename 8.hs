compress :: Eq a => [a] -> [a]
compress (a:b:xs) = if a == b
                    then compress (b:xs)
                    else a:compress (b:xs)
compress [] = []
compress (a:[]) = [a]
