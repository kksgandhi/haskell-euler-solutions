import Macros
import Data.Numbers.Primes
import Data.List

main = do
  print $ maximum perms
  where
    n = 7
    perms = filter isPrime $ map unSplit $ permutations [1..n]
