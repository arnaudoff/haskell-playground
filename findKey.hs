-- Useful if we represent associations as list of pairs
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k then Just v else findKey key xs

findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key =
    foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing
