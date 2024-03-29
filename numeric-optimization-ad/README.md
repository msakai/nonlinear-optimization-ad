# numeric-optimization-ad

[![Hackage](https://img.shields.io/hackage/v/numeric-optimization-ad.svg)](https://hackage.haskell.org/package/numeric-optimization-ad)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/numeric-optimization-ad.svg)](https://packdeps.haskellers.com/feed?needle=numeric-optimization-ad)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Wrapper of [numeric-optimization](https://hackage.haskell.org/package/numeric-optimization) package for using with [ad](https://hackage.haskell.org/package/ad) package

## Example Usage

```haskell
{-# LANGUAGE FlexibleContexts #-}
import Numeric.Optimization
import Numeric.Optimization.AD

main :: IO ()
main = do
  result <- minimize LBFGS def (UsingReverse rosenbrock) [-3,-4]
  print (resultSuccess result)  -- True
  print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
  print (resultValue result)  -- 1.8129771632403013e-18

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: Floating a => [a] -> a
-- rosenbrock :: Reifies s Tape => [Reverse s Double] -> Reverse s Double
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)

sq :: Floating a => a -> a
sq x = x ** 2
```
