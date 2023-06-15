# numeric-optimization

[![Hackage](https://img.shields.io/hackage/v/numeric-optimization.svg)](https://hackage.haskell.org/package/numeric-optimization)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/numeric-optimization.svg)](https://packdeps.haskellers.com/feed?needle=numeric-optimization)
[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

Unified interface to various numerical optimization algorithms.

The aim of the package is to provide a convenient interface like Python's [scipy.optimize](https://docs.scipy.org/doc/scipy/reference/optimize.html).

Note that the package name is numeric-optimization and not numeri**cal**-optimization.
The name `numeric-optimization` comes from the module name `Numeric.Optimization`.


## Example Usage

```haskell
{-# LANGUAGE OverloadedLists #-}
import Data.Vector.Storable (Vector)
import Numeric.Optimization

main :: IO ()
main = do
  result <- minimize LBFGS def (WithGrad rosenbrock rosenbrock') [-3,-4]
  print (resultSuccess result)  -- True
  print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
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
|Limited memory BFGS with bounds constraints (L-BFGS-B)|[L-BFGS-B](http://users.iems.northwestern.edu/~nocedal/lbfgsb.html)|[l-bfgs-b](https://hackage.haskell.org/package/l-bfgs-b)|Requires `with-lbfgsb` flag|
|Newton's method|Pure Haskell implementation using [HMatrix](https://hackage.haskell.org/package/hmatrix)|-|

## Installation

### Installing Prerequisites

#### BLAS and LAPACK

You may need to install BLAS and LAPACK for `hmatrix`.

Windows (MSYS2):
```
$ pacman -S mingw-w64-x86_64-lapack
```

or if you use MSYS2 installed by `stack`

```
$ stack exec -- pacman -S mingw-w64-x86_64-lapack
```

#### liblbfgsb

If you want to use L-BFGS-B, you have to install the development package of `liblbfgsb`.

Ubuntu Linux:
```
$ apt-get install liblbfgsb-dev
```

Homebrew (macOS and Linux): 
```
$ brew install msakai/tap/liblbfgsb
```

Windows (MSYS2):
```
$ wget https://github.com/msakai/mingw-w64-liblbfgsb/releases/download/v3.0-1/mingw-w64-x86_64-liblbfgsb-3.0-1-any.pkg.tar.zst
$ pacman -U mingw-w64-x86_64-liblbfgsb-3.0-1-any.pkg.tar.zst
```

or if you use MSYS2 installed by `stack`

```
$ wget https://github.com/msakai/mingw-w64-liblbfgsb/releases/download/v3.0-1/mingw-w64-x86_64-liblbfgsb-3.0-1-any.pkg.tar.zst
$ stack exec -- pacman -Sy
$ stack exec -- pacman -U mingw-w64-x86_64-liblbfgsb-3.0-1-any.pkg.tar.zst
```

## Related Packages

* Packages for using with automatic differentiation:
  * [numeric-optimization-ad](https://hackage.haskell.org/package/numeric-optimization-ad) for using with [ad](https://hackage.haskell.org/package/ad) package
  * [numeric-optimization-backprop](https://hackage.haskell.org/package/numeric-optimization-backprop) for using with [backprop](https://hackage.haskell.org/package/backprop) package
* [MIP](https://hackage.haskell.org/package/MIP) for solving linear programming and mixed-integer linear programming problems

## LICENSE

The code in thie packaged is licensed under [BSD-3-Clause](LIENSE).

If you enable `with-cg-descent` flag, it uses GPL-licensed packages and the resulting binary should be distributed under GPL.
