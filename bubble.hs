bubble :: (Ord a) => [a] -> [a]
bubble a = bubble' [] a False
  where
    bubble' prev next mov
      | (length next) < 2 = if mov == True then bubble prev ++ next  else prev ++ next
      | h > t = bubble' (prev ++ [t])  ([h] ++ (drop 2 next)) True
      | otherwise = bubble' (prev ++ [h]) (drop 1 next) mov
        where
          h = (head next)
          t = (head (tail next)) 
       
main = print $ bubble [4,5,5,7,8,9,2,5,0]  
