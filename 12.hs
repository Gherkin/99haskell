data Entry a = Single a | Multiple Int a deriving (Show)

decodeModified :: Eq a => [Entry a] -> [a]
decodeModified [] = []
decodeModified (x:[]) = decodeEntry x
decodeModified (x:xs) = decodeEntry x ++ decodeModified xs

decodeEntry :: Eq a => Entry a -> [a]
decodeEntry (Single x) = [x]
decodeEntry (Multiple n x) = multiplyElem x n

multiplyElem :: a -> Int -> [a]
multiplyElem x 1 = [x]
multiplyElem x n = x:multiplyElem x (n - 1)

