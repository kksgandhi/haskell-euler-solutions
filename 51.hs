import Macros
import Data.Numbers.Primes

hasDigits :: Int -> Int -> [Int] -> Bool
hasDigits howMany digit num = (length $ filter (== digit) num) == howMany

splitInt :: Int -> [Int]
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

replace :: [Int] -> Int -> Int -> [Int]
replace num toReplace replacement = map (\x -> if x == toReplace then replacement else x) num

findFamily :: [Int] -> Int -> [[Int]]
findFamily num digit = filter (isPrime . unSplit) $ map (replace num digit) [0..9]

main = do
  print $ take 10 bigFamilies
  where
    digit         = 2
    numDigits     = 3
    splitPrimes   = map splitInt primes
    digitalPrimes = filter (hasDigits numDigits digit) splitPrimes
    primeFamilies = map (flip findFamily digit) digitalPrimes
    bigFamilies   = filter ((>=8) . length) primeFamilies
