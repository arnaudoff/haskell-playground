delete' :: (Eq a) => a -> [a] -> [a]
delete' deleted xs = [ x | x <- xs, x /= deleted ]

intersect' :: (Eq a) => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' (x:xs) ys
      | x `elem` ys = x : intersect' xs (delete' x ys)
      | otherwise = intersect' xs ys
