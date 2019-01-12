mapMatrix :: [[a]] -> (a -> a) -> [[a]]
mapMatrix xxs f = map (\xs -> map f xs) xxs
