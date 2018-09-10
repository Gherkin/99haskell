myButLast :: [a] -> a
myButLast xs = let ys = tail xs
               in let zs = tail ys
                  in if null zs
                     then head xs
                     else myButLast ys
