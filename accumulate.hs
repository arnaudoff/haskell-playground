accumulate :: (Integral a) =>
    (a -> a -> a) -> a -> a -> a -> (a -> a) -> (a -> a) -> a
accumulate op nullValue start end term next
  | start > end = nullValue
  | otherwise = op (term start) (accumulate op nullValue (next start) end term next)
