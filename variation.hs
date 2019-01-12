accumulate :: (Integral a) => (a -> a -> a) -> a -> a -> a -> (a -> a) -> (a -> a) -> a
accumulate op nullValue start end term next
  | start > end = nullValue
  | otherwise = op (term start) (accumulate op nullValue (next start) end term next)

factorial :: (Integral a) => a -> a
factorial n = accumulate (*) 1 1 n id (\x -> x + 1)

variation :: (Integral a) => a -> a -> a
variation k n = (factorial n) `div` factorial (n - k)
