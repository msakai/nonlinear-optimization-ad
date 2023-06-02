# numeric-optimization

Unified interface to various numerical optimization algorithms.

Note that the package name is numeric-optimization and not numeri**cal**-optimization.
The name `numeric-optimization` comes from the module name `Numeric.Optimization`.


## Example Usage

```haskell
{-# LANGUAGE OverloadedLists #-}

import Data.Vector.Storable (Vector)
import Numeric.Optimization

main :: IO ()
main = do
  (x, result, stat) <- minimize LBFGS def (WithGrad rosenbrock rosenbrock') [-3,-4]
  print x  -- [0.999999999009131,0.9999999981094296]
  print (resultSuccess result)  -- True
  print (resultValue result)  -- 1.8129771632403013e-18

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: Vector Double -> Double
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)

rosenbrock' :: Vector Double -> Vector Double
rosenbrock' [x,y] =
  [ 2 * (1 - x) * (-1) + 100 * 2 * (y - sq x) * (-2) * x
  , 100 * 2 * (y - sq x)
  ]

sq :: Floating a => a -> a
sq x = x ** 2
```

## Supported Algorithms

|Algorithm|Solver implemention|Haskell binding| |
|---------|-------------------|---------------|-|
|CG\_DESCENT|[CG_DESCENT-C](https://www.math.lsu.edu/~hozhang/SoftArchive/CG_DESCENT-C-3.0.tar.gz)|[nonlinear-optimization](https://hackage.haskell.org/package/nonlinear-optimization)|Requires `with-cg-descent` flag|
|Limited memory BFGS (L-BFGS)|[liblbfgs](https://github.com/chokkan/liblbfgs)|[lbfgs](https://hackage.haskell.org/package/lbfgs)|


## Related Packages

* Packages for using with automatic differentiation:
  * [numerical-optimization-ad](https://hackage.haskell.org/package/numerical-optimization-ad) for using with [ad](https://hackage.haskell.org/package/ad) package
  * [numerical-optimization-backprop](https://hackage.haskell.org/package/numerical-optimization-backprop) for using with [backprop](https://hackage.haskell.org/package/backprop) package
* [MIP](https://hackage.haskell.org/package/MIP) for solving linear programming and mixed-integer linear programming problems

## LICENSE

The code in thie packaged is licensed under [BSD-3-Clause](LIENSE).

If you enable `with-cg-descent` flag, it uses GPL-licensed packages and the resulting binary should be distributed under GPL.
