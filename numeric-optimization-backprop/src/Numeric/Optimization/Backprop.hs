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
  , Method (..)
  , Params (..)
  , Result (..)
  , Statistics (..)
  , OptimizationException (..)
  , ToVector

  -- * Problem definition
  , Constraint (..)

  -- * Re-exports
  , Default (..)
  , module Numeric.Backprop
  ) where


import Data.Default.Class
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import Numeric.Backprop
import qualified Numeric.Optimization as Opt
import Numeric.Optimization hiding (minimize, Params (..), IsProblem (..))
import Numeric.Optimization.Backprop.ToVector


-- | Parameters for optimization algorithms
data Params a
  = Params
  { callback :: Maybe (a -> IO Bool)
    -- ^ If callback returns @True@, the algorithm execution is terminated.
  }

instance Default (Params f) where
  def =
    Params
    { callback = Nothing
    }


data Problem a
  = Problem
      (forall s. Reifies s W => BVar s a -> BVar s Double)
      (V.Vector (Double, Double))
      [Constraint]
      a


instance (Backprop a, ToVector a) => Opt.IsProblem (Problem a) where
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
minimize
  :: forall a. (Backprop a, ToVector a)
  => Method  -- ^ Numerical optimization algorithm to use
  -> Params a  -- ^ Parameters for optimization algorithms. Use 'def' as a default.
  -> (forall s. Reifies s W => BVar s a -> BVar s Double)  -- ^ Function to be minimized.
  -> Maybe [(Double, Double)]  -- ^ Bounds
  -> [Constraint]  -- ^ Constraints
  -> a -- ^ Initial value
  -> IO (a, Result, Statistics)
minimize method params f bounds constraints x0 = do
  let bounds' :: V.Vector (Double, Double)
      bounds' =
        case bounds of
          Nothing -> VG.replicate (dim x0) (-1/0, 1/0)
          Just bs -> VG.fromList bs

      prob :: Problem a
      prob = Problem f bounds' constraints x0

      params' :: Opt.Params
      params' =
        Opt.Params
        { Opt.callback = fmap (\cb -> cb . updateFromVector x0) (callback params)
        }

  (x, result, stat) <- Opt.minimize method params' prob (toVector x0)
  return (updateFromVector x0 x, result, stat)
