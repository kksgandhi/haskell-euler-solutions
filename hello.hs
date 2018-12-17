main = mapM_ print $ bubbleSort [10,12,42,112,57,111,1,1,1111,0,5,31415, -5]

bubbleSort :: (Ord a) => [a] -> [[a]]
bubbleSort x = (take (length x)) $ iterate bubble x
  where bubble (x:y:r)
          | x <= y    = x : bubble (y:r)
          | otherwise = y : bubble (x:r)
        bubble x = x
