# Changelog for `numeric-optimization`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.2.0.0 - Unreleased

* Introduced assosiated type `Domain` to the `IsProblem` type class:
  Now `Domain prob` instead of `Vector Double` is used to represent
  inputs and gradients.
* Move `numeric-optimization-backprop`'s `Numeric.Optimization.Backprop.ToVector`
  to `numeric-optimization`'s `Numeric.Optimization.Utils.ToVector`.
* Now we have `instance ToVector a => IsProblem (a -> Double)`, so that
  `Double -> Double`, `(Double, Double) -> Double`, `[Double] -> Double`, etc.
  are all instances of `IsProblem`.
* Add `Numeric.Optimization.Utils.AllClose`. This module is primarily intended to
  be used for testing in this package and its family packages.


## 0.1.1.0 - 2023-06-21

* Support L-BFGS-B algorithm (when `with-lbfgsb` is enabled)
* Add some algorithm specific parameters
* Add instructions for installing dependent libraries
* Add `with-lbfgs` flag, which is `true` by default, but you can turn-off
  the flag to build without L-BFGS.
* Add some instances of standard type classes: `Eq OptimizationException`,
  `Show Result`, and `Show Statistics`.
* Return correct statistics for L-BFGS and L-BFGS-B.
* Fix many bugs

## 0.1.0.1 - 2023-06-03

* Various documentation fixes
* Fix `build-examples` flag to work

## 0.1.0.0 - 2023-06-03

* Initial release.
