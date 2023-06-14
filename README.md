# nonlinear-optimization-ad

[![build](https://github.com/msakai/nonlinear-optimization-ad/actions/workflows/build.yaml/badge.svg)](https://github.com/msakai/nonlinear-optimization-ad/actions/workflows/build.yaml)

This repository contains several Haskell packages for numerical optimizations.

## nonlinear-optimization

Packages for using [nonlinear-optimization](https://hackage.haskell.org/package/nonlinear-optimization)
with automatic differentiation.

* [nonlinear-optimization-ad](nonlinear-optimization-ad/) for using with [ad](https://hackage.haskell.org/package/ad) package
* [nonlinear-optimization-backprop](nonlinear-optimization-backprop/) for using with [ad](https://hackage.haskell.org/package/backprop) package

## numeric-optimization

Packages for using various optimization algorithms not limited to *CG Descent* provided by  `nonlinear-optimization`.

* [numeric-optimization](numeric-optimization/) - Unified interface to various numerical optimization algorithms.
* [numeric-optimization-ad](numeric-optimization-ad/) for using `numeric-optimization` with [ad](https://hackage.haskell.org/package/ad) package
* [numeric-optimization-backprop](numeric-optimization-backprop/) for using `numeric-optimization` with [ad](https://hackage.haskell.org/package/backprop) package
