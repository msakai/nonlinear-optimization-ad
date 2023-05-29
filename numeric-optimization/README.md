# numeric-optimization

Unified interface to various numerical optimization algorithms.

Note that the package name is numeric-optimization and not numeri**cal**-optimization.
The name `numeric-optimization` comes from the module name `Numeric.Optimization`.

## Supported Algorithms

|Algorithm|Solver implemention|Haskell binding| |
|---------|-------------------|---------------|-|
|CG\_DESCENT|[CG_DESCENT-C](https://www.math.lsu.edu/~hozhang/SoftArchive/CG_DESCENT-C-3.0.tar.gz)|[nonlinear-optimization](https://hackage.haskell.org/package/nonlinear-optimization)|Requires `with-cg-descent` flag|
|Limited memory BFGS (L-BFGS)|[liblbfgs](https://github.com/chokkan/liblbfgs)|[lbfgs](https://hackage.haskell.org/package/lbfgs)|


## Related Packages

There are also companion packages for using with automatic differentiation:

* `numerical-optimization-ad` for using with [ad](https://hackage.haskell.org/package/ad) package
* `numerical-optimization-backprop` for using with [backprop](https://hackage.haskell.org/package/backprop) package

## LICENSE

The code in thie packaged is licensed under [BSD-3-Clause](LIENSE).

If you enable `with-cg-descent` flag, it uses GPL-licensed packages and the resulting binary should be distributed under GPL.
