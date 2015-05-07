qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
  let smaller = filter (<= x) xs
      bigger = filter (> x) xs
  in
    (qsort smaller) ++ [x] ++ (qsort bigger)
    
main = print $ qsort [1,23,43,3,456,4,2,3,45] 
