{-# LANGUAGE ViewPatterns #-}
isPalindrome :: Eq a => [a] -> Bool

isPalindrome (a:[]) = True
isPalindrome (a:b:[]) = a == b
isPalindrome (xs) = let (h, bs, t) = splitHeadTail xs
                    in h == t && isPalindrome bs

splitHeadTail :: [a] -> (a, [a], a)
splitHeadTail xs = let (bs, t) = splitTail xs
                   in appendTuple (splitHead bs) t

splitTail :: [a] -> ([a], a)
splitTail xs = let rs = reverse xs 
               in swap (head rs, reverse $ tail rs)

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x, xs)

swap :: (a, b) -> (b, a)
swap (x, y) = (y,x)

appendTuple :: (a, b) -> c -> (a, b, c)
appendTuple (a, b) c = (a, b, c)
