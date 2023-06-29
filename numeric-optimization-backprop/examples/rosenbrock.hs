{-# LANGUAGE FlexibleContexts #-}
import Numeric.Optimization
import Numeric.Optimization.Backprop
import Lens.Micro

main :: IO ()
main = do
  result <- minimize LBFGS def (UsingBackprop rosenbrock) (-3,-4)
  print (resultSuccess result)  -- True
  print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
  print (resultValue result)  -- 1.8129771632403013e-18

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: Reifies s W => BVar s (Double, Double) -> BVar s Double
rosenbrock t = sq (1 - x) + 100 * sq (y - sq x)
  where
    x = t ^^. _1
    y = t ^^. _2

sq :: Floating a => a -> a
sq x = x ** 2
