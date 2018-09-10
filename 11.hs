data Entry a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [Entry a]
encodeModified [] = []
encodeModified (x:[]) = [Single x]
encodeModified (x:xs) = reverse $ encodeModified' [Single x] xs

encodeModified' :: Eq a => [Entry a] -> [a] -> [Entry a]
encodeModified' (x:xs) (y:[]) = encodeEntry x y ++ xs
encodeModified' (x:xs) (y:ys) = encodeModified' (encodeEntry x y ++ xs) ys

encodeEntry :: Eq a => Entry a -> a -> [Entry a]
encodeEntry (Single x) y = if x == y then
                               [Multiple 2 x]
                           else
                               [Single y, Single x]
encodeEntry (Multiple n x) y = if x == y then
                                   [Multiple (n + 1) x]
                               else
                                   [Single y, Multiple n x]

