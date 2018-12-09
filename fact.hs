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
