digits :: Int -> [Int]
digits n = dg n []
  where
    dg 0 x = x
    dg n x = dg (quot n 10) $ (mod n 10):x

happy :: Int -> Int
happy n = hpp (digits n)
 where
  hpp [] = 0
  hpp (x:xs) = (x*x) + (hpp xs)

is_happy :: Int -> Bool
is_happy n = ihpp n []
  where
    ihpp a x = if a == 1 then True else if (elem a x) then False else ihpp (happy a) (a:x)

