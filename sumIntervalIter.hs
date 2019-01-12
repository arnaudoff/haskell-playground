sumIntervalIter :: Int -> Int -> Int
sumIntervalIter x y = loop x y 0
    where
        loop :: Int -> Int -> Int -> Int
        loop s e sum = if s > e then sum else loop (s + 1) e (sum + s)
