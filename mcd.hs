--Euler
mcd :: Int -> Int -> Int
mcd m n
  | r == 0 = n
  | otherwise = mcd n r
  where r = mod m n

main = print $ mcd 10 3
