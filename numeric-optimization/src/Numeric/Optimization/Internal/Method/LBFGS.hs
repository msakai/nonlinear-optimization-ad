{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Internal.Method.LBFGS
-- Copyright   :  (c) Masahiro Sakai 2023-2024
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Numeric.Optimization.Internal.Method.LBFGS
  ( isSupported
  , minimize
  ) where

import Control.Exception
import Data.Vector.Storable (Vector)
import Numeric.Optimization.Internal.Base

#ifdef WITH_LBFGS
import Control.Applicative
import Data.Coerce
import Data.IORef
import Data.Maybe
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.C
import qualified Numeric.LBFGS.Vector as LBFGS
import qualified Numeric.LBFGS.Raw as LBFGS (unCLBFGSResult, lbfgserrCanceled)
#endif


#ifndef WITH_LBFGS

isSupported :: Bool
isSupported = False

minimize :: HasGrad prob => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize _params _prob _ = throwIO (UnsupportedMethod LBFGS)

#else

isSupported :: Bool
isSupported = True

minimize :: HasGrad prob => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "LBFGS does not support bounds")
minimize _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "LBFGS does not support constraints")
minimize params prob x0 = do
  evalCounter <- newIORef (0::Int)
  iterRef <- newIORef (0::Int)

  let lbfgsParams =
        LBFGS.LBFGSParameters
        { LBFGS.lbfgsPast = paramsPast params
        , LBFGS.lbfgsDelta = fromMaybe 1e-5 $ paramsFTol params <|> paramsTol params
        , LBFGS.lbfgsLineSearch = LBFGS.DefaultLineSearch
        , LBFGS.lbfgsL1NormCoefficient = Nothing
        }

      instanceData :: ()
      instanceData = ()

      evalFun :: () -> VSM.IOVector CDouble -> VSM.IOVector CDouble -> CInt -> CDouble -> IO CDouble
      evalFun _inst xvec gvec _n _step = do
        modifyIORef' evalCounter (+1)
#if MIN_VERSION_vector(0,13,0)
        x <- VG.unsafeFreeze (VSM.unsafeCoerceMVector xvec :: VSM.IOVector Double)
        y <- grad'M prob x (VSM.unsafeCoerceMVector gvec :: VSM.IOVector Double)
#else
        x <- VG.unsafeFreeze (coerce xvec :: VSM.IOVector Double)
        y <- grad'M prob x (coerce gvec :: VSM.IOVector Double)
#endif
        return (coerce y)

      progressFun :: () -> VSM.IOVector CDouble -> VSM.IOVector CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CInt -> CInt -> CInt -> IO CInt
      progressFun _inst xvec _gvec _fx _xnorm _gnorm _step _n iter _nev = do
        writeIORef iterRef $! fromIntegral iter
        shouldStop <-
          case paramsCallback params of
            Nothing -> return False
            Just callback -> do
#if MIN_VERSION_vector(0,13,0)
              x <- VG.freeze (VSM.unsafeCoerceMVector xvec :: VSM.IOVector Double)
#else
              x <- VG.freeze (coerce xvec :: VSM.IOVector Double)
#endif
              callback x
        return $ if shouldStop then LBFGS.unCLBFGSResult LBFGS.lbfgserrCanceled else 0

  (result, x_) <- LBFGS.lbfgs lbfgsParams evalFun progressFun instanceData (VG.toList x0)
  let x = VG.fromList x_
      (success, msg) =
        case result of
          LBFGS.Success                -> (True,  "Success")
          LBFGS.Stop                   -> (True,  "Stop")
          LBFGS.AlreadyMinimized       -> (True,  "The initial variables already minimize the objective function.")
          LBFGS.UnknownError           -> (False, "Unknown error.")
          LBFGS.LogicError             -> (False, "Logic error.")
          LBFGS.OutOfMemory            -> (False, "Insufficient memory.")
          LBFGS.Canceled               -> (False, "The minimization process has been canceled.")
          LBFGS.InvalidN               -> (False, "Invalid number of variables specified.")
          LBFGS.InvalidNSSE            -> (False, "Invalid number of variables (for SSE) specified.")
          LBFGS.InvalidXSSE            -> (False, "The array x must be aligned to 16 (for SSE).")
          LBFGS.InvalidEpsilon         -> (False, "Invalid parameter lbfgs_parameter_t::epsilon specified.")
          LBFGS.InvalidTestPeriod      -> (False, "Invalid parameter lbfgs_parameter_t::past specified.")
          LBFGS.InvalidDelta           -> (False, "Invalid parameter lbfgs_parameter_t::delta specified.")
          LBFGS.InvalidLineSearch      -> (False, "Invalid parameter lbfgs_parameter_t::linesearch specified.")
          LBFGS.InvalidMinStep         -> (False, "Invalid parameter lbfgs_parameter_t::max_step specified.")
          LBFGS.InvalidMaxStep         -> (False, "Invalid parameter lbfgs_parameter_t::max_step specified.")
          LBFGS.InvalidFtol            -> (False, "Invalid parameter lbfgs_parameter_t::ftol specified.")
          LBFGS.InvalidWolfe           -> (False, "Invalid parameter lbfgs_parameter_t::wolfe specified.")
          LBFGS.InvalidGtol            -> (False, "Invalid parameter lbfgs_parameter_t::gtol specified.")
          LBFGS.InvalidXtol            -> (False, "Invalid parameter lbfgs_parameter_t::xtol specified.")
          LBFGS.InvalidMaxLineSearch   -> (False, "Invalid parameter lbfgs_parameter_t::max_linesearch specified.")
          LBFGS.InvalidOrthantwise     -> (False, "Invalid parameter lbfgs_parameter_t::orthantwise_c specified.")
          LBFGS.InvalidOrthantwiseStart-> (False, "Invalid parameter lbfgs_parameter_t::orthantwise_start specified.")
          LBFGS.InvalidOrthantwiseEnd  -> (False, "Invalid parameter lbfgs_parameter_t::orthantwise_end specified.")
          LBFGS.OutOfInterval          -> (False, "The line-search step went out of the interval of uncertainty.")
          LBFGS.IncorrectTMinMax       -> (False, "A logic error occurred; alternatively, the interval of uncertainty became too small.")
          LBFGS.RoundingError          -> (False, "A rounding error occurred; alternatively, no line-search step satisfies the sufficient decrease and curvature conditions.")
          LBFGS.MinimumStep            -> (False, "The line-search step became smaller than lbfgs_parameter_t::min_step.")
          LBFGS.MaximumStep            -> (False, "The line-search step became larger than lbfgs_parameter_t::max_step.")
          LBFGS.MaximumLineSearch      -> (False, "The line-search routine reaches the maximum number of evaluations.")
          LBFGS.MaximumIteration       -> (False, "The algorithm routine reaches the maximum number of iterations.")
          LBFGS.WidthTooSmall          -> (False, "Relative width of the interval of uncertainty is at most lbfgs_parameter_t::xtol.")
          LBFGS.InvalidParameters      -> (False, "A logic error (negative line-search step) occurred.")
          LBFGS.IncreaseGradient       -> (False, "The current search direction increases the objective function value.")

  iters <- readIORef iterRef
  nEvals <- readIORef evalCounter

  return $
    Result
    { resultSuccess = success
    , resultMessage = msg
    , resultSolution = x
    , resultValue = func prob x
    , resultGrad = Nothing
    , resultHessian = Nothing
    , resultHessianInv = Nothing
    , resultStatistics =
        Statistics
        { totalIters = iters
        , funcEvals = nEvals + 1  -- +1 is for computing 'resultValue'
        , gradEvals = nEvals
        , hessEvals = 0
        , hessianEvals = 0
        }
    }

#endif
