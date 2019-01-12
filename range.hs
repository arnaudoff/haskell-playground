range' :: (Integral a) => a -> a -> [a]
range' start end =
    if start > end then [] else start : range' (start + 1) end
