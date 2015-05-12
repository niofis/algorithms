fib :: Int -> Int
fib x
  | x < 3 = 1
  |otherwise = (fib (x-1)) + (fib (x-2))

main = print $ fib 5
