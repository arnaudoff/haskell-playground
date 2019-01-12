accumulate :: (Integral a) => (a -> a -> a) -> a -> a -> a -> (a -> a) -> (a -> a) -> a
accumulate op nullValue start end term next
  | start > end = nullValue
  | otherwise = op (term start) (accumulate op nullValue (next start) end term next)

factorial :: (Integral a) => a -> a
factorial n = accumulate (*) 1 1 n id (\x -> x + 1)

combination :: (Integral a) => a -> a -> a
combination k n = (factorial n) `div` (factorial (n - k) * factorial k)
