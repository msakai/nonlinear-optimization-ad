import Numeric.Optimization

main :: IO ()
main = do
  result <- minimize LBFGS def (WithGrad rosenbrock rosenbrock') (-3,-4)
  print (resultSuccess result)  -- True
  print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
  print (resultValue result)  -- 1.8129771632403013e-18

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: (Double, Double) -> Double
rosenbrock (x,y) = sq (1 - x) + 100 * sq (y - sq x)

rosenbrock' :: (Double, Double) -> (Double, Double)
rosenbrock' (x,y) =
  ( 2 * (1 - x) * (-1) + 100 * 2 * (y - sq x) * (-2) * x
  , 100 * 2 * (y - sq x)
  )

sq :: Floating a => a -> a
sq x = x ** 2
