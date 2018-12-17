module Macros (
takeDropWhile ,
onlyTake      ,
unfinite      ,
deepMap       ,
deeepMap      ,
splitInt      ,
unSplit       ,
hasDigits     ,
replace       ,
count
) where

count :: (Ord a) => a -> [a] -> Int
count x xs = (length . filter (== x)) xs

takeDropWhile :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
takeDropWhile dropper taker = (dropWhile (dropper)) . (takeWhile (taker))

onlyTake :: (a -> Bool) -> [a] -> [a]
onlyTake taker a = takeDropWhile (not . taker) taker a

unfinite :: Int -> [[b]] -> [[b]]
unfinite a b = take a $ map (take a) b

deepMap :: (a -> b) -> [[a]] -> [[b]]
deepMap f a = map (map f) a

deeepMap :: (a -> b) -> [[[a]]] -> [[[b]]]
deeepMap f a = map (deepMap f) a


hasDigits :: Int -> Int -> [Int] -> Bool
hasDigits howMany digit num = (length $ filter (== digit) num) == howMany

replace :: [Int] -> Int -> Int -> [Int]
replace num toReplace replacement = map (\x -> if x == toReplace then replacement else x) num

splitInt :: Integral a => a -> [a]
splitInt num = reverse $ reverseSplitNum num
  where
    reverseSplitNum mun
      | mun == 0  = []
      | otherwise = (mun `mod` 10) : (reverseSplitNum (mun  `div` 10))

unSplit :: [Int] -> Int
unSplit num = reverseUnSplit (reverse num)
  where 
    reverseUnSplit [] = 0
    reverseUnSplit (x:xs) = x + (10 * reverseUnSplit xs)
