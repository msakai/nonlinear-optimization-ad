{-# LANGUAGE FlexibleContexts #-}
import Numeric.Optimization.AD

main :: IO ()
main = do
  result <- minimize LBFGS def rosenbrock Nothing [] [-3,-4]
  print (resultSuccess result)  -- True
  print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
  print (resultValue result)  -- 1.8129771632403013e-18

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: Floating a => [a] -> a
-- rosenbrock :: Reifies s Tape => [Reverse s Double] -> Reverse s Double
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)

sq :: Floating a => a -> a
sq x = x ** 2
