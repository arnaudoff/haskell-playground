reduce' :: (Num a) => (a -> a -> a) -> [a] -> a -> a
reduce' f [] start = start
reduce' f (x:xs) start = f x (reduce' f xs start)
