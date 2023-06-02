{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  -- * Main function
    minimize

  -- * Problem specification
  , Constraint (..)

  -- * Algorithm selection
  , Method (..)
  , isSupportedMethod
  , Params (..)

  -- * Result
  , Result (..)
  , Statistics (..)
  , OptimizationException (..)

  -- * Utilities and Re-exports
  , Default (..)
  , ToVector
  , module Numeric.Backprop
  ) where


import Data.Default.Class
import Data.Functor.Contravariant
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import Numeric.Backprop
import qualified Numeric.Optimization as Opt
import Numeric.Optimization hiding (minimize, IsProblem (..))
import Numeric.Optimization.Backprop.ToVector


data Problem a
  = Problem
      (forall s. Reifies s W => BVar s a -> BVar s Double)
      (Maybe (V.Vector (Double, Double)))
      [Constraint]
      a


instance (ToVector a) => Opt.IsProblem (Problem a) where
  func (Problem f _bounds _constraints x0) x = evalBP f (updateFromVector x0 x)

  bounds (Problem _f bounds _constraints _template) = bounds

  constraints (Problem _f _bounds constraints _template) = constraints


instance (Backprop a, ToVector a) => Opt.HasGrad (Problem a) where
  grad (Problem f _bounds _constraints x0) x = toVector $ gradBP f (updateFromVector x0 x)

  grad'M (Problem f _bounds _constraints x0) x gvec = do
    case backprop f (updateFromVector x0 x) of
      (y, g) -> do
        writeToMVector g gvec
        return y


instance (Backprop a, ToVector a) => Opt.Optionally (Opt.HasGrad (Problem a)) where
  optionalDict = hasOptionalDict


instance Opt.Optionally (Opt.HasHessian (Problem a)) where
  optionalDict = Nothing


-- | Minimization of scalar function of one or more variables.
--
-- This is a wrapper of 'Opt.minimize' and use "Numeric.Backprop" to compute gradient.
--
-- Example:
--
-- > {-# LANGUAGE FlexibleContexts #-}
-- > import Numeric.Optimization.Backprop
-- > import Lens.Micro
-- > 
-- > main :: IO ()
-- > main = do
-- >   (x, result, stat) <- minimize LBFGS def rosenbrock Nothing [] (-3,-4)
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
minimize
  :: forall a. (Backprop a, ToVector a)
  => Method  -- ^ Numerical optimization algorithm to use
  -> Params a  -- ^ Parameters for optimization algorithms. Use 'def' as a default.
  -> (forall s. Reifies s W => BVar s a -> BVar s Double)  -- ^ Function to be minimized.
  -> Maybe [(Double, Double)]  -- ^ Bounds
  -> [Constraint]  -- ^ Constraints
  -> a -- ^ Initial value
  -> IO (Result a)
minimize method params f bounds constraints x0 = do
  let bounds' :: Maybe (V.Vector (Double, Double))
      bounds' = fmap VG.fromList bounds

      prob :: Problem a
      prob = Problem f bounds' constraints x0

      params' :: Params (VS.Vector Double)
      params' = contramap (updateFromVector x0) params

  result <- Opt.minimize method params' prob (toVector x0)
  return $ fmap (updateFromVector x0) result
