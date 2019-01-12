filterMatrix :: [[a]] -> (a -> Bool) -> [[a]]
filterMatrix xxs f = map (\xs -> filter f xs) xxs
