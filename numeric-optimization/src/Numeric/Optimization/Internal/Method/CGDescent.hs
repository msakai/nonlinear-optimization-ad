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
-- Module      :  Numeric.Optimization.Internal.Method.CGDescent
-- Copyright   :  (c) Masahiro Sakai 2023-2024
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Numeric.Optimization.Internal.Method.CGDescent
  ( isSupported
  , minimize
  ) where

import Control.Exception
import Data.Vector.Storable (Vector)
import Numeric.Optimization.Internal.Base

#ifdef WITH_CG_DESCENT
import Control.Monad.Primitive
import Data.Maybe
import qualified Data.Vector.Generic as VG
import qualified Numeric.Optimization.Algorithms.HagerZhang05 as CG
#endif


#ifndef WITH_CG_DESCENT

isSupported :: Bool
isSupported = False

minimize :: HasGrad prob => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize _params _prob _ = throwIO (UnsupportedMethod CGDescent)

#else

isSupported :: Bool
isSupported = True

minimize :: HasGrad prob => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "CGDescent does not support bounds")
minimize _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "CGDescent does not support constraints")
minimize params prob x0 = do
  let grad_tol = fromMaybe 1e-6 $ paramsTol params

      cg_params =
        CG.defaultParameters
        { CG.printFinal = False
        , CG.maxItersFac =
            case paramsMaxIters params of
              Nothing -> CG.maxItersFac CG.defaultParameters
              Just m -> fromIntegral m / fromIntegral (VG.length x0)
        }

      mf :: forall m. PrimMonad m => CG.PointMVector m -> m Double
      mf mx = do
        x <- VG.unsafeFreeze mx
        return $ func prob x

      mg :: forall m. PrimMonad m => CG.PointMVector m -> CG.GradientMVector m -> m ()
      mg mx mret = do
        x <- VG.unsafeFreeze mx
        _ <- grad'M prob x mret
        return ()

      mc :: forall m. PrimMonad m => CG.PointMVector m -> CG.GradientMVector m -> m Double
      mc mx mret = do
        x <- VG.unsafeFreeze mx
        grad'M prob x mret

  (x, result, stat) <-
    CG.optimize
      cg_params
      grad_tol
      x0
      (CG.MFunction mf)
      (CG.MGradient mg)
      (Just (CG.MCombined mc))

  let (success, msg) =
        case result of
          CG.ToleranceStatisfied      -> (True, "convergence tolerance satisfied")
          CG.FunctionChange           -> (True, "change in func <= feps*|f|")
          CG.MaxTotalIter             -> (False, "total iterations exceeded maxit")
          CG.NegativeSlope            -> (False, "slope always negative in line search")
          CG.MaxSecantIter            -> (False, "number secant iterations exceed nsecant")
          CG.NotDescent               -> (False, "search direction not a descent direction")
          CG.LineSearchFailsInitial   -> (False, "line search fails in initial interval")
          CG.LineSearchFailsBisection -> (False, "line search fails during bisection")
          CG.LineSearchFailsUpdate    -> (False, "line search fails during interval update")
          CG.DebugTol                 -> (False, "debugger is on and the function value increases")
          CG.FunctionValueNaN         -> (False, "function value became nan")
          CG.StartFunctionValueNaN    -> (False, "starting function value is nan")

  return $
    Result
    { resultSuccess = success
    , resultMessage = msg
    , resultSolution = x
    , resultValue = CG.finalValue stat
    , resultGrad = Nothing
    , resultHessian = Nothing
    , resultHessianInv = Nothing
    , resultStatistics =
        Statistics
        { totalIters = fromIntegral $ CG.totalIters stat
        , funcEvals = fromIntegral $ CG.funcEvals stat
        , gradEvals = fromIntegral $ CG.gradEvals stat
        , hessEvals = 0
        , hessianEvals = 0
        }
    }

#endif
