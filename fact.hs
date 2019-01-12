fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fact' :: Int -> Int
fact' n
    | n == 0 = 1
    | n > 0  = n * fact'(n - 1)

factiter :: Int -> Int
factiter n = iter n 1 1

iter :: Int -> Int -> Int -> Int
iter n index result =
    if index > n then result else iter n (index + 1) (result * index)

accumulate :: (Integral a) =>
    (a -> a -> a) -> a -> a -> a -> (a -> a) -> (a -> a) -> a
accumulate op nullValue start end term next
  | start > end = nullValue
  | otherwise = op (term start) (accumulate op nullValue (next start) end term next)

factAccumulate :: (Integral a) => a -> a
factAccumulate n = accumulate (*) 1 1 n id (\x -> x + 1)
