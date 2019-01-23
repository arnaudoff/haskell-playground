customDiv :: Int -> Int -> Maybe Int
customDiv n m
    | (m == 0) = Nothing
    | otherwise = Just (n `div` m)
