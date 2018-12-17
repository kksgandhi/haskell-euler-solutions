import Macros
import Data.Function

totient :: Integral a => a -> [a]
totient a = filter ((== 1) . (gcd a)) [1..a]

main = do
  print nOver
  where
    totients = map (length . totient) [2..1000000]
    nOver = zipWith ((/) `on` fromIntegral) totients  [2..1000000]
