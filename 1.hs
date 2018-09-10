myLast :: [a] -> a
myLast xs = let y = tail xs
            in if null y
               then head xs
               else myLast y

