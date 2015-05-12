bubble :: (Ord a) => [a] -> [a]
bubble a = bubble' [] a 0
  where
    bubble' prev next mov
      | (length next) < 2 = if mov == 1 then bubble prev ++ next  else prev ++ next
      | h > t = bubble' (prev ++ [t])  ([h] ++ (drop 2 next)) 1
      | otherwise = bubble' (prev ++ [h]) (drop 1 next) mov
        where
          temp = (take 2 next)
          h = (head temp)
          t = (head (tail temp)) 
       
main = print $ bubble [4,5,5,7,8,9,2,5,0]  
