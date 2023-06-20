# Changelog for `numeric-optimization`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## 0.1.1.0 - Unreleased

* Support L-BFGS-B algorithm (when `with-lbfgsb` is enabled)
* Add some algorithm specific parameters
* Add instructions for installing dependent libraries
* Add `with-lbfgs` flag, which is `true` by default, but you can turn-off
  the flag to build without L-BFGS.
* Add some instances of standard type classes: `Eq OptimizationException`,
  `Show Result`, and `Show Statistics`.
* Return correct statistics for L-BFGS and L-BFGS-B.

## 0.1.0.1 - 2023-06-03

* Various documentation fixes
* Fix `build-examples` flag to work

## 0.1.0.0 - 2023-06-03

* Initial release.
