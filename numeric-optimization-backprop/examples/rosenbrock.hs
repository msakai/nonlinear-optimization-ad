{-# LANGUAGE FlexibleContexts #-}
import Numeric.Optimization.Backprop
import Lens.Micro

main :: IO ()
main = do
  (x, result, stat) <- minimize LBFGS def rosenbrock Nothing [] (-3,-4)
  print x  -- [0.999999999009131,0.9999999981094296]
  print (resultSuccess result)  -- True
  print (resultValue result)  -- 1.8129771632403013e-18

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: Reifies s W => BVar s (Double, Double) -> BVar s Double
rosenbrock t = sq (1 - x) + 100 * sq (y - sq x)
  where
    x = t ^^. _1
    y = t ^^. _2

sq :: Floating a => a -> a
sq x = x ** 2
