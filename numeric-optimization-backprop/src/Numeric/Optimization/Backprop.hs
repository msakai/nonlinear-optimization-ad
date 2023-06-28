{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Backprop
-- Copyright   :  (c) Masahiro Sakai 2023
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module is a wrapper of "Numeric.Optimization" that uses
-- [backprop](https://hackage.haskell.org/package/backprop)'s automatic differentiation.
--
-----------------------------------------------------------------------------
module Numeric.Optimization.Backprop
  (
  -- * Problem specification
    UsingBackprop (..)

  -- * Utilities and Re-exports
  , ToVector
  , module Numeric.Backprop
  ) where


import Numeric.Backprop
import Numeric.Optimization
import Numeric.Optimization.Backprop.ToVector


-- | Type for defining function and its gradient using automatic differentiation
-- provided by "Numeric.Backprop".
--
-- Example:
--
-- > {-# LANGUAGE FlexibleContexts #-}
-- > import Numeric.Optimization
-- > import Numeric.Optimization.Backprop
-- > import Lens.Micro
-- >
-- > main :: IO ()
-- > main = do
-- >   result <- minimize LBFGS def (UsingBackprop rosenbrock) (-3,-4)
-- >   print (resultSuccess result)  -- True
-- >   print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
-- >   print (resultValue result)  -- 1.8129771632403013e-18
-- >
-- > -- https://en.wikipedia.org/wiki/Rosenbrock_function
-- > rosenbrock :: Reifies s W => BVar s (Double, Double) -> BVar s Double
-- > rosenbrock t = sq (1 - x) + 100 * sq (y - sq x)
-- >   where
-- >     x = t ^^. _1
-- >     y = t ^^. _2
-- >
-- > sq :: Floating a => a -> a
-- > sq x = x ** 2
data UsingBackprop a
  = UsingBackprop (forall s. Reifies s W => BVar s a -> BVar s Double)


instance (ToVector a) => IsProblem (UsingBackprop a) where
  type Domain (UsingBackprop a) = a

  fromDomain _ = toVector

  fromDomainM _ = writeToMVector

  toDomain _ = updateFromVector

  func (UsingBackprop f) x = evalBP f x

  bounds (UsingBackprop _f) = Nothing

  constraints (UsingBackprop _f) = []


instance (Backprop a, ToVector a) => HasGrad (UsingBackprop a) where
  grad (UsingBackprop f) x = gradBP f x

  grad'M (UsingBackprop f) x gvec = do
    case backprop f x of
      (y, g) -> do
        writeToMVector g gvec
        return y


instance (Backprop a, ToVector a) => Optionally (HasGrad (UsingBackprop a)) where
  optionalDict = hasOptionalDict


instance Optionally (HasHessian (UsingBackprop a)) where
  optionalDict = Nothing
