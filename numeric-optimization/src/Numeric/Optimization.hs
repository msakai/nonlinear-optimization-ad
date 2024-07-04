{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization
-- Copyright   :  (c) Masahiro Sakai 2023
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module aims to provide unified interface to various numerical
-- optimization, like [scipy.optimize](https://docs.scipy.org/doc/scipy/reference/optimize.html) in Python.
--
-- In this module, you need to explicitly provide the function to calculate the
-- gradient, but you can use
-- [numeric-optimization-ad](https://hackage.haskell.org/package/numeric-optimization-ad),
-- [numeric-optimization-ad-delcont](https://hackage.haskell.org/package/numeric-optimization-ad-delcont), or
-- [numeric-optimization-backprop](https://hackage.haskell.org/package/numeric-optimization-backprop)
-- to define it using automatic differentiation.
--
-----------------------------------------------------------------------------
module Numeric.Optimization
  (

  -- * Main function
    minimize

  -- * Problem specification
  --
  -- $problemDefinition
  , IsProblem (..)
  , HasGrad (..)
  , HasHessian (..)
  , Constraint (..)
  , boundsUnconstrained
  , isUnconstainedBounds
  -- ** Wrapper types
  , WithGrad (..)
  , WithHessian (..)
  , WithBounds (..)
  , WithConstraints (..)

  -- * Algorithm selection
  , Method (..)
  , isSupportedMethod
  , Params (..)

  -- * Result
  , Result (..)
  , Statistics (..)
  , OptimizationException (..)

  -- * Utilities and Re-export
  , Default (..)
  , Optionally (..)
  , hasOptionalDict
  ) where

import Control.Exception
import Control.Monad
import Data.Constraint (Dict (..))
import Data.Default.Class
import Data.Functor.Contravariant
import Data.Maybe
import Data.Proxy
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as VG
import qualified Numeric.LinearAlgebra as LA
import Numeric.Optimization.Internal.Base
import qualified Numeric.Optimization.Internal.Method.CGDescent as CGDescent
import qualified Numeric.Optimization.Internal.Method.LBFGS as LBFGS
import qualified Numeric.Optimization.Internal.Method.LBFGSB as LBFGSB


-- | Whether a 'Method' is supported under the current environment.
isSupportedMethod :: Method -> Bool
isSupportedMethod LBFGS = LBFGS.isSupported
isSupportedMethod CGDescent = CGDescent.isSupported
isSupportedMethod LBFGSB = LBFGSB.isSupported
isSupportedMethod Newton = True


-- $problemDefinition
--
-- Problems are specified by types of 'IsProblem' type class.
--
-- A problem consists of an objective function and constraints.
--
-- You can use the functions of following type, for example, as
-- unconstrained problems:
--
-- * Double -> Double
--
-- * (Double, Double) -> Doule
--
-- * (Double, Double, Double) -> Doule
--
-- * …
--
-- * 'VS.Vector' Double -> Double
--
-- Many optimization algorithms in this module ('Method') require
-- gradient and/or hessian. In this case you can add those information
-- using wrapper types: 'WithGrad' and 'WithHessian'. For example:
--
-- > (\x -> x**2) `'WithGrad' (\x -> 2*x)
--
-- If your problem is a constrained problem. You can add constraints
-- using the wrappers: 'WithBounds' and 'WithConstraints'. For example:
--
-- > (\(x,y) -> x**2 + y**2) `WithBounds` ((-1,-2), (1,2))
--
-- You can use [numeric-optimization-ad](https://hackage.haskell.org/package/numeric-optimization-ad),
-- [numeric-optimization-ad-delcont](https://hackage.haskell.org/package/numeric-optimization-ad-delcont),
-- or [numeric-optimization-backprop](https://hackage.haskell.org/package/numeric-optimization-backprop)
-- to avoid hand-writing functions for computing gradients and hesians.
--
-- If you need further flexibility, you can define instance of
-- 'IsProblem' by yourself.


-- | Minimization of scalar function of one or more variables.
--
-- This function is intended to provide functionality similar to Python's @scipy.optimize.minimize@.
--
-- Example:
--
-- > import Numeric.Optimization
-- >
-- > main :: IO ()
-- > main = do
-- >   result <- minimize LBFGS def (WithGrad rosenbrock rosenbrock') (-3,-4)
-- >   print (resultSuccess result)  -- True
-- >   print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
-- >   print (resultValue result)  -- 1.8129771632403013e-18
-- >
-- > -- https://en.wikipedia.org/wiki/Rosenbrock_function
-- > rosenbrock :: (Double, Double) -> Double
-- > rosenbrock (x,y) = sq (1 - x) + 100 * sq (y - sq x)
-- >
-- > rosenbrock' :: (Double, Double) -> (Double, Double)
-- > rosenbrock' (x,y) =
-- >   ( 2 * (1 - x) * (-1) + 100 * 2 * (y - sq x) * (-2) * x
-- >   , 100 * 2 * (y - sq x)
-- >   )
-- >
-- > sq :: Floating a => a -> a
-- > sq x = x ** 2
minimize
  :: forall prob. (IsProblem prob, Optionally (HasGrad prob), Optionally (HasHessian prob))
  => Method  -- ^ Numerical optimization algorithm to use
  -> Params (Domain prob) -- ^ Parameters for optimization algorithms. Use 'def' as a default.
  -> prob  -- ^ Optimization problem to solve
  -> Domain prob  -- ^ Initial value
  -> IO (Result (Domain prob))
minimize method params prob x0 = do
  let x0' = toVector (Proxy :: Proxy prob) x0
  ret <- minimizeV method (contramap (updateFromVector (Proxy :: Proxy prob) x0) params) (AsVectorProblem prob x0) x0'
  return $ fmap (updateFromVector (Proxy :: Proxy prob) x0) ret

minimizeV
  :: forall prob. (IsProblem prob, Optionally (HasGrad prob), Optionally (HasHessian prob))
  => Method  -- ^ Numerical optimization algorithm to use
  -> Params (Vector Double) -- ^ Parameters for optimization algorithms. Use 'def' as a default.
  -> AsVectorProblem prob  -- ^ Optimization problem to solve
  -> Vector Double  -- ^ Initial value
  -> IO (Result (Vector Double))
minimizeV CGDescent =
  case optionalDict @(HasGrad prob) of
    Just Dict -> CGDescent.minimize
    Nothing -> \_ _ _ -> throwIO GradUnavailable
minimizeV LBFGS =
  case optionalDict @(HasGrad prob) of
    Just Dict -> LBFGS.minimize
    Nothing -> \_ _ _ -> throwIO GradUnavailable
minimizeV LBFGSB =
  case optionalDict @(HasGrad prob) of
    Just Dict -> LBFGSB.minimize
    Nothing -> \_ _ _ -> throwIO GradUnavailable
minimizeV Newton =
  case optionalDict @(HasGrad prob) of
    Nothing -> \_ _ _ -> throwIO GradUnavailable
    Just Dict ->
      case optionalDict @(HasHessian prob) of
        Nothing -> \_ _ _ -> throwIO HessianUnavailable
        Just Dict -> minimize_Newton
minimizeV method = \_ _ _ -> throwIO (UnsupportedMethod method)


minimize_Newton :: (HasGrad prob, HasHessian prob) => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize_Newton _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "Newton does not support bounds")
minimize_Newton _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "Newton does not support constraints")
minimize_Newton params prob x0 = do
  let tol = fromMaybe 1e-6 (paramsTol params)

      loop !x !y !g !h !iter = do
        shouldStop <- msum <$> sequence
          [ pure $ case paramsMaxIters params of
              Just maxIter | maxIter <= iter -> Just "maximum number of iterations reached"
              _ -> Nothing
          , case paramsCallback params of
              Nothing -> return Nothing
              Just callback -> do
                flag <- callback x
                return $ if flag then Just "The minimization process has been canceled." else Nothing
          ]
        case shouldStop of
          Just reason ->
            return $
              Result
              { resultSuccess = False
              , resultMessage = reason
              , resultSolution = x
              , resultValue = y
              , resultGrad = Just g
              , resultHessian = Just h
              , resultHessianInv = Nothing
              , resultStatistics =
                  Statistics
                  { totalIters = iter
                  , funcEvals = iter + 1
                  , gradEvals = iter + 1
                  , hessEvals = iter + 1
                  , hessianEvals = iter + 1
                  }
              }
          Nothing -> do
            let p = h LA.<\> g
                x' = VG.zipWith (-) x p
            if LA.norm_Inf (VG.zipWith (-) x' x) > tol then do
              let (y', g') = grad' prob x'
                  h' = hessian prob x'
              loop x' y' g' h' (iter + 1)
            else do
              return $
                Result
                { resultSuccess = True
                , resultMessage = "success"
                , resultSolution = x
                , resultValue = y
                , resultGrad = Just g
                , resultHessian = Just h
                , resultHessianInv = Nothing
                , resultStatistics =
                    Statistics
                    { totalIters = iter
                    , funcEvals = iter + 1
                    , gradEvals = iter + 1
                    , hessEvals = iter + 1
                    , hessianEvals = iter + 1
                    }
                }

  let (y0, g0) = grad' prob x0
      h0 = hessian prob x0
  loop x0 y0 g0 h0 0
