slice :: (Num a, Eq a, Ord a) => [a] -> a -> a -> [a]
slice xs start end = loop xs start end 0 [] False
    where
        loop :: (Num a, Eq a, Ord a) => [a] -> a -> a -> a -> [a] -> Bool -> [a]
        loop [] _ _ _ result _ = result
        loop (x:xs) start end current result inside
          | current == start && not inside = loop xs start end (current + 1) (x : result) True
          | current == end && inside = reverse (x : result)
          | current < end && inside = loop xs start end (current + 1) (x: result) True
          | otherwise = loop xs start end (current + 1) result False
