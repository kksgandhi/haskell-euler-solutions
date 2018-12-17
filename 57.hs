import Macros
import Data.NumberLength

iter :: Integer -> Integer -> (Integer, Integer)
iter a b = (x, y)
  where
    preX = a + b
    preY = b
    fliX = preY
    fliY = preX
    x    = fliY + fliX
    y    = fliY

main = do
  print $ length goodIters
  where
    iterations = take 1000 $ iterate (uncurry iter) (3,2)
    goodIters  = filter (uncurry (\x y -> numberLength x > numberLength y)) iterations
