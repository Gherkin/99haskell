myLength :: [a] -> Int
myLength xs = if null xs
              then 0
              else myLength (tail xs) + 1
