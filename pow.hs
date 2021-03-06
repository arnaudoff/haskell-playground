pow' :: (Num a, Eq a) => a -> a -> a
pow' x n
    | n == 0 = 1
    | otherwise = x * pow' x (n - 1)

accumulate :: (Integral a) => (a -> a -> a) -> a -> a -> a -> (a -> a) -> (a -> a) -> a
accumulate op nullValue start end term next
  | start > end = nullValue
  | otherwise = op (term start) (accumulate op nullValue (next start) end term next)

powAccumulate :: (Integral a) => a -> a -> a
powAccumulate a n = accumulate (*) 1 1 n (\_ -> a) (\x -> x + 1)
