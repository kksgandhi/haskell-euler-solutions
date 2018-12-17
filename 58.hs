import Macros
import Data.Numbers.Primes

main = do
  print $ ((+ 1) . length) $ takeWhile (>0.1) fracsAtN
  print $ take 5 fracsAtN
  where
    n         = 10
    numNums   = 4 * n + 1
    baseNums  = [1..]
    fNums     = map (\x -> 4*x + 1) [1.0..]
    upRight   = map (fromEnum . isPrime . (\x -> 4*(x^2) - 2*x + 1)) baseNums
    upLeft    = map (fromEnum . isPrime . (\x -> 4*(x^2)       + 1)) baseNums
    downLeft  = map (fromEnum . isPrime . (\x -> 4*(x^2) + 2*x + 1)) baseNums
    primesAtN = zipWith3 (\x y z -> x + y + z) upRight upLeft downLeft
    sumPrims  = map fromIntegral $ scanl1 (+) primesAtN
    fracsAtN  = zipWith (/) sumPrims fNums
