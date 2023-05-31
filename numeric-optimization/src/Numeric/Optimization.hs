{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
-- This module aims to provides unifined interface to various numerical
-- optimization, like [scipy.optimize](https://docs.scipy.org/doc/scipy/reference/optimize.html) in Python.
--
-- In this module, you need to explicitly provide the function to calculate the
-- gradient, -- but you can use @numeric-optimization-ad@ or
-- @numeric-optimization-backprop@ to define it using automatic differentiation.
--
-----------------------------------------------------------------------------
module Numeric.Optimization
  (

  -- * Main function
    minimize
  , Method (..)
  , isSupportedMethod
  , Params (..)
  , Result (..)
  , Statistics (..)
  , OptimizationException (..)

  -- * Problem definition
  , IsProblem (..)
  , HasGrad (..)
  , HasHessian (..)
  , Optionally (..)
  , hasOptionalDict
  , Constraint (..)
  , boundsUnconstrained
  , isUnconstainedBounds

  -- * Re-exports
  , Default (..)
  ) where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.Constraint (Dict (..))
import Data.Default.Class
import Data.IORef
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.C
import qualified Numeric.LBFGS.Vector as LBFGS
#ifdef WITH_CG_DESCENT
import qualified Numeric.Optimization.Algorithms.HagerZhang05 as CG
#endif
import Numeric.LinearAlgebra (Matrix)
import qualified Numeric.LinearAlgebra as LA


-- | Selection of numerical optimization algorithms
data Method
  = CGDescent
    -- ^ Conjugate gradient method based on Hager and Zhang [1].
    --
    -- The implementation is provided by nonlinear-optimization package [3]
    -- which is a binding library of [2].
    --
    -- * [1] Hager, W. W. and Zhang, H.  /A new conjugate gradient/
    --   /method with guaranteed descent and an efficient line/
    --   /search./ Society of Industrial and Applied Mathematics
    --   Journal on Optimization, 16 (2005), 170-192.
    --
    -- * [2] <https://www.math.lsu.edu/~hozhang/SoftArchive/CG_DESCENT-C-3.0.tar.gz>
    --
    -- * [3] <https://hackage.haskell.org/package/nonlinear-optimization>
  | LBFGS
    -- ^ Limited memory BFGS (L-BFGS) algorithm [1]
    --
    -- The implementtion is provided by lbfgs package [2]
    -- which is a binding of liblbfgs [3].
    --
    -- * [1] <https://en.wikipedia.org/wiki/Limited-memory_BFGS>
    --
    -- * [2] <https://hackage.haskell.org/package/lbfgs>
    --
    -- * [3] <https://github.com/chokkan/liblbfgs>
  deriving (Eq, Ord, Enum, Show, Bounded)


-- | Whether a 'Method' is supported under the current environment.
isSupportedMethod :: Method -> Bool
isSupportedMethod LBFGS = True
#ifdef WITH_CG_DESCENT
isSupportedMethod CGDescent = True
#else
isSupportedMethod CGDescent = False
#endif


-- | Parameters for optimization algorithms
data Params a
  = Params
  { callback :: Maybe (a -> IO Bool)
    -- ^ If callback returns @True@, the algorithm execution is terminated.
  }

instance Default (Params a) where
  def =
    Params
    { callback = Nothing
    }


-- | Optimization result
data Result
  = Result
  { resultSuccess :: Bool
    -- ^ Whether or not the optimizer exited successfully.
  , resultMessage :: String
    -- ^ Description of the cause of the termination.
  , resultValue :: Double
    -- ^ Value of the function at the solution.
  , resultGrad :: Vector Double
    -- ^ Gradient at the solution
  , resultHess :: Maybe (Matrix Double)
    -- ^ Hessian at the solution; may be an approximation.
  , resultHessInv :: Maybe (Matrix Double)
    -- ^ Inverse of Hessian at the solution; may be an approximation.
  }


-- | Statistics of optimizaion process
data Statistics
  = Statistics
  { totalIters :: Int
    -- ^ Total number of iterations.
  , funcEvals :: Int
    -- ^ Total number of function evaluations.
  , gradEvals :: Int
    -- ^ Total number of gradient evaluations.
  , hessEvals :: Int
    -- ^ Total number of hessian evaluations.
  }


-- | The bad things that can happen when you use the library.
data OptimizationException
  = UnsupportedProblem String
  | UnsupportedMethod Method
  | GradUnavailable
  | HessianUnavailable
  deriving (Show)

instance Exception OptimizationException


-- | Optimization problems
class IsProblem prob where
  -- | Objective function
  --
  -- It is called @fun@ in @scipy.optimize.minimize@.
  func :: prob -> Vector Double -> Double

  -- | Bounds
  --
  bounds :: prob -> Maybe (V.Vector (Double, Double))
  bounds _ = Nothing

  -- | Constraints
  constraints :: prob -> [Constraint]
  constraints _ = []

  {-# MINIMAL func #-}


-- | Optimization problem equipped with gradient information
class IsProblem prob => HasGrad prob where
  -- | Gradient of a function computed by 'func'
  --
  -- It is called @jac@ in @scipy.optimize.minimize@.
  grad :: prob -> Vector Double -> Vector Double
  grad prob = snd . grad' prob

  -- | Pair of 'func' and 'grad'
  grad' :: prob -> Vector Double -> (Double, Vector Double)
  grad' prob x = runST $ do
    gret <- VGM.new (VG.length x)
    y <- grad'M prob x gret
    g <- VG.unsafeFreeze gret
    return (y, g)

  -- | Similar to 'grad'' but destination passing style is used for gradient vector
  grad'M :: PrimMonad m => prob -> Vector Double -> VSM.MVector (PrimState m) Double -> m Double
  grad'M prob x gvec = do
    let y = func prob x
    VG.imapM_ (VGM.write gvec) (grad prob x)
    return y

  {-# MINIMAL grad | grad' | grad'M #-}


-- | Optimization problem equipped with hessian information
class IsProblem prob => HasHessian prob where
  -- | Hessian of a function computed by 'func'
  --
  -- It is called @hess@ in @scipy.optimize.minimize@.
  hessian :: prob -> Vector Double -> Matrix Double

  -- | The product of the hessian @H@ of a function @f@ at @x@ with a vector @x@.
  --
  -- It is called @hessp@ in @scipy.optimize.minimize@.
  --
  -- See also <https://hackage.haskell.org/package/ad-4.5.4/docs/Numeric-AD.html#v:hessianProduct>.
  hessianProduct :: prob -> Vector Double -> Vector Double -> Vector Double
  hessianProduct prob x v = hessian prob x LA.#> v

  {-# MINIMAL hessian #-}


-- | Optional constraint
class Optionally c where
  optionalDict :: Maybe (Dict c)


-- | Utility function to define 'Optionally' instances
hasOptionalDict :: c => Maybe (Dict c)
hasOptionalDict = Just Dict


-- | Type of constraint
--
-- Currently, no constraints are supported.
data Constraint

-- | Bounds for unconstrained problems, i.e. (-∞,+∞).
boundsUnconstrained :: Int -> V.Vector (Double, Double)
boundsUnconstrained n = V.replicate n (-1/0, 1/0)

-- | Whether all lower bounds are -∞ and all upper bounds are +∞.
isUnconstainedBounds :: V.Vector (Double, Double) -> Bool
isUnconstainedBounds = V.all p
  where
    p (lb, ub) = isInfinite lb && lb < 0 && isInfinite ub && ub > 0


-- | Minimization of scalar function of one or more variables.
--
-- This function is intended to provide functionality similar to Python's @scipy.optimize.minimize@.
minimize
  :: forall prob. (IsProblem prob, Optionally (HasGrad prob), Optionally (HasHessian prob))
  => Method  -- ^ Numerical optimization algorithm to use
  -> Params (Vector Double) -- ^ Parameters for optimization algorithms. Use 'def' as a default.
  -> prob  -- ^ Optimization problem to solve
  -> Vector Double  -- ^ Initial value
  -> IO (Vector Double, Result, Statistics)
#ifdef WITH_CG_DESCENT
minimize CGDescent =
  case optionalDict @(HasGrad prob) of
    Just Dict -> minimize_CGDescent
    Nothing -> \_ _ _ -> throwIO GradUnavailable
#endif
minimize LBFGS =
  case optionalDict @(HasGrad prob) of
    Just Dict -> minimize_LBFGS
    Nothing -> \_ _ _ -> throwIO GradUnavailable
minimize method = \_ _ _ -> throwIO (UnsupportedMethod method)


#ifdef WITH_CG_DESCENT

minimize_CGDescent :: HasGrad prob => Params (Vector Double) -> prob -> Vector Double -> IO (Vector Double, Result, Statistics)
minimize_CGDescent _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "CGDescent does not support bounds")
minimize_CGDescent _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "CGDescent does not support constraints")
minimize_CGDescent _params prob x0 = do
  let grad_tol = 1e-6

      cg_params = CG.defaultParameters

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

  return
    ( x
    , Result
      { resultSuccess = success
      , resultMessage = msg
      , resultValue = CG.finalValue stat
      , resultGrad = grad prob x
      , resultHess = Nothing
      , resultHessInv = Nothing
      }
    , Statistics
      { totalIters = fromIntegral $ CG.totalIters stat
      , funcEvals = fromIntegral $ CG.funcEvals stat
      , gradEvals = fromIntegral $ CG.gradEvals stat
      , hessEvals = 0
      }
    )

#endif


minimize_LBFGS :: HasGrad prob => Params (Vector Double) -> prob -> Vector Double -> IO (Vector Double, Result, Statistics)
minimize_LBFGS _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "LBFGS does not support bounds")
minimize_LBFGS _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "LBFGS does not support constraints")
minimize_LBFGS params prob x0 = do
  evalCounter <- newIORef (0::Int)
  iterRef <- newIORef (0::Int)

  let lbfgsParams =
        LBFGS.LBFGSParameters
        { LBFGS.lbfgsPast = Nothing
        , LBFGS.lbfgsDelta = 0
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
          case callback params of
            Nothing -> return False
            Just f -> do
#if MIN_VERSION_vector(0,13,0)
              x <- VG.freeze (VSM.unsafeCoerceMVector xvec :: VSM.IOVector Double)
#else
              x <- VG.freeze (coerce xvec :: VSM.IOVector Double)
#endif
              f x
        return $ if shouldStop then 1 else 0

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
      (y, g) = grad' prob x

  nEvals <- readIORef evalCounter

  return
    ( x
    , Result
      { resultSuccess = success
      , resultMessage = msg
      , resultValue = y
      , resultGrad = g
      , resultHess = Nothing
      , resultHessInv = Nothing
      }
    , Statistics
      { totalIters = undefined
      , funcEvals = nEvals + 1
      , gradEvals = nEvals + 1
      , hessEvals = 0
      }
    )


-- ------------------------------------------------------------------------

instance IsProblem (Vector Double -> Double) where
  func f = f

instance Optionally (HasGrad (Vector Double -> Double)) where
  optionalDict = Nothing

instance Optionally (HasHessian (Vector Double -> Double)) where
  optionalDict = Nothing
