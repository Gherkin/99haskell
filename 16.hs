dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs k = dropEvery' xs k 1

dropEvery' :: [a]  -> Int -> Int -> [a]
dropEvery' [] _ _ = []
dropEvery' (x:xs) k n = if n `mod` k == 0 then
                            dropEvery' xs k (n + 1)
                        else
                            x:dropEvery' xs k (n + 1)
