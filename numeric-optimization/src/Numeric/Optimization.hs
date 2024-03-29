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

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Coerce
import Data.Constraint (Dict (..))
import Data.Default.Class
import Data.Functor.Contravariant
import Data.IORef
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Foreign.C
#ifdef WITH_LBFGS
import qualified Numeric.LBFGS.Vector as LBFGS
import qualified Numeric.LBFGS.Raw as LBFGS (unCLBFGSResult, lbfgserrCanceled)
#endif
#ifdef WITH_CG_DESCENT
import qualified Numeric.Optimization.Algorithms.HagerZhang05 as CG
#endif
import Numeric.Optimization.Utils.ToVector (ToVector)
import qualified Numeric.Optimization.Utils.ToVector as ToVector
#ifdef WITH_LBFGSB
import qualified Numeric.LBFGSB as LBFGSB
import qualified Numeric.LBFGSB.Result as LBFGSB
#endif
import Numeric.Limits
import Numeric.LinearAlgebra (Matrix)
import qualified Numeric.LinearAlgebra as LA
import System.IO.Unsafe


-- | Selection of numerical optimization algorithms
data Method
  = CGDescent
    -- ^ Conjugate gradient method based on Hager and Zhang [1].
    --
    -- The implementation is provided by nonlinear-optimization package [3]
    -- which is a binding library of [2].
    --
    -- This method requires gradient but does not require hessian.
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
    -- This method requires gradient but does not require hessian.
    --
    -- * [1] <https://en.wikipedia.org/wiki/Limited-memory_BFGS>
    --
    -- * [2] <https://hackage.haskell.org/package/lbfgs>
    --
    -- * [3] <https://github.com/chokkan/liblbfgs>
  | LBFGSB
    -- ^ Limited memory BFGS algorithm with bound constraints (L-BFGS-B) [1][2][3]
    --
    -- The implementation is provided by l-bfgs-b package [4]
    -- which is a bindign to L-BFGS-B fortran code [5].
    --
    -- * [1] R. H. Byrd, P. Lu and J. Nocedal. [A Limited Memory Algorithm for Bound Constrained Optimization](http://www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gz), (1995), SIAM Journal on Scientific and Statistical Computing , 16, 5, pp. 1190-1208.
    --
    -- * [2] C. Zhu, R. H. Byrd and J. Nocedal. [L-BFGS-B: Algorithm 778: L-BFGS-B, FORTRAN routines for large scale bound constrained optimization](http://www.ece.northwestern.edu/~nocedal/PSfiles/lbfgsb.ps.gz) (1997), ACM Transactions on Mathematical Software, Vol 23, Num. 4, pp. 550-560.
    --
    -- * [3] J. L. Morales and J. Nocedal. [L-BFGS-B: Remark on Algorithm 778: L-BFGS-B, FORTRAN routines for large scale bound constrained optimization](http://www.ece.northwestern.edu/~morales/PSfiles/acm-remark.pdf) (2011), ACM Transactions on Mathematical Software, Vol 38, Num. 7, pp. 1–4
    --
    -- * [4] <https://hackage.haskell.org/package/l-bfgs-b>
    --
    -- * [5] <http://users.iems.northwestern.edu/~nocedal/lbfgsb.html>
    --
    -- @since 0.1.1.0
  | Newton
    -- ^ Naïve implementation of Newton method in Haskell
    --
    -- This method requires both gradient and hessian.
  deriving (Eq, Ord, Enum, Show, Bounded)


-- | Whether a 'Method' is supported under the current environment.
isSupportedMethod :: Method -> Bool
#ifdef WITH_LBFGS
isSupportedMethod LBFGS = True
#else
isSupportedMethod LBFGS = False
#endif
#ifdef WITH_CG_DESCENT
isSupportedMethod CGDescent = True
#else
isSupportedMethod CGDescent = False
#endif
#ifdef WITH_LBFGSB
isSupportedMethod LBFGSB = True
#else
isSupportedMethod LBFGSB = False
#endif
isSupportedMethod Newton = True


-- | Parameters for optimization algorithms
--
-- TODO:
--
-- * Better way to pass algorithm specific parameters?
--
-- * Separate 'paramsCallback' from other more concrete serializeable parameters?
data Params a
  = Params
  { paramsCallback :: Maybe (a -> IO Bool)
    -- ^ If callback function returns @True@, the algorithm execution is terminated.
  , paramsTol :: Maybe Double
    -- ^ Tolerance for termination. When @tol@ is specified, the selected algorithm sets
    -- some relevant solver-specific tolerance(s) equal to @tol@.
    --
    -- If specified, this value is used as defaults for 'paramsFTol' and 'paramsGTol'.
  , paramsFTol :: Maybe Double
    -- ^ 'LBFGS' stops iteration when delta-based convergence test
    -- (see 'paramsPast') is enabled and the following condition is
    -- met:
    --
    -- \[
    --     \left|\frac{f' - f}{f}\right| < \mathrm{ftol},
    -- \]
    --
    -- where @f'@ is the objective value of @past@ ('paramsPast') iterations ago,
    -- and @f@ is the objective value of the current iteration.
    -- The default value is @1e-5@.
    --
    -- 'LBFGSB' stops iteration when the following condition is met:
    --
    -- \[
    --     \frac{f^k - f^{k+1}}{\mathrm{max}\{|f^k|,|f^{k+1}|,1\}} \le \mathrm{ftol}.
    -- \]
    --
    -- The default value is @1e7 * ('epsilon' :: Double) = 2.220446049250313e-9@.
    --
    -- @since 0.1.1.0
  , paramsGTol :: Maybe Double
    -- ^ 'LBFGSB' stops iteration when \(\mathrm{max}\{|\mathrm{pg}_i| \mid i = 1, \ldots, n\} \le \mathrm{gtol}\)
    -- where \(\mathrm{pg}_i\) is the i-th component of the projected gradient.
    --
    -- @since 0.1.1.0
  , paramsMaxIters :: Maybe Int
    -- ^ Maximum number of iterations.
    --
    -- Currently only 'LBFGSB', 'CGDescent', and 'Newton' uses this.
    --
    -- @since 0.1.1.0
  , paramsPast :: Maybe Int
    -- ^ Distance for delta-based convergence test in 'LBFGS'
    --
    -- This parameter determines the distance, in iterations, to compute
    -- the rate of decrease of the objective function. If the value of this
    -- parameter is @Nothing@, the library does not perform the delta-based
    -- convergence test. The default value is @Nothing@.
    --
    -- @since 0.1.1.0
  , paramsMaxCorrections :: Maybe Int
    -- ^ The maximum number of variable metric corrections used in 'LBFGSB'
    -- to define the limited memory matrix.
    --
    -- @since 0.1.1.0
  }

instance Default (Params a) where
  def =
    Params
    { paramsCallback = Nothing
    , paramsTol = Nothing
    , paramsFTol = Nothing
    , paramsGTol = Nothing
    , paramsMaxIters = Nothing
    , paramsPast = Nothing
    , paramsMaxCorrections = Nothing
    }

instance Contravariant Params where
  contramap f params =
    params
    { paramsCallback = fmap ((. f)) (paramsCallback params)
    }


-- | Optimization result
data Result a
  = Result
  { resultSuccess :: Bool
    -- ^ Whether or not the optimizer exited successfully.
  , resultMessage :: String
    -- ^ Description of the cause of the termination.
  , resultSolution :: a
    -- ^ Solution
  , resultValue :: Double
    -- ^ Value of the function at the solution.
  , resultGrad :: Maybe a
    -- ^ Gradient at the solution
  , resultHessian :: Maybe (Matrix Double)
    -- ^ Hessian at the solution; may be an approximation.
  , resultHessianInv :: Maybe (Matrix Double)
    -- ^ Inverse of Hessian at the solution; may be an approximation.
  , resultStatistics :: Statistics
    -- ^ Statistics of optimizaion process
  }
  deriving (Show)

instance Functor Result where
  fmap f result =
    result
    { resultSolution = f (resultSolution result)
    , resultGrad = fmap f (resultGrad result)
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
  , hessianEvals :: Int
    -- ^ Total number of hessian evaluations.
  , hessEvals :: Int
    -- ^ Total number of hessian evaluations.
  }
  deriving (Show)

{-# DEPRECATED hessEvals "Use hessianEvals instead" #-}


-- | The bad things that can happen when you use the library.
data OptimizationException
  = UnsupportedProblem String
  | UnsupportedMethod Method
  | GradUnavailable
  | HessianUnavailable
  deriving (Show, Eq)

instance Exception OptimizationException



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
-- > import Data.Vector (fromList)
-- >
-- > (\(x,y) -> x**2 + y**2) `WithBounds` (fromList [(-1,1), (-2,2)])
--
-- You can use [numeric-optimization-ad](https://hackage.haskell.org/package/numeric-optimization-ad),
-- [numeric-optimization-ad-delcont](https://hackage.haskell.org/package/numeric-optimization-ad-delcont),
-- or [numeric-optimization-backprop](https://hackage.haskell.org/package/numeric-optimization-backprop)
-- to avoid hand-writing functions for computing gradients and hesians.
--
-- If you need further flexibility, you can define instance of
-- 'IsProblem' by yourself.

-- | Optimization problems
--
-- Laws that should be satisfied:
--
-- * @'VS.length' . 'toVector' prob = 'dim' prob@
--
-- * @'updateFromVector' prob a ('toVector' prob a) = a@
--
-- * @'updateFromVector' prob ('updateFromVector' prob a v1) v2 = 'updateFromVector' prob a v2@
class IsProblem prob where
  -- | Type of input values and gradients
  --
  -- @since 0.2.0.0
  type Domain prob

  -- | Dimention of a @'Domain' prob@ value.
  --
  -- @since 0.2.0.0
  dim :: prob -> Domain prob -> Int
  dim prob x = VS.length $ toVector prob x

  -- | Convert a @'Domain' prob@ value to a storable 'Vector'.
  --
  -- @since 0.2.0.0
  toVector :: prob -> Domain prob -> Vector Double
  toVector prob x = VS.create $ do
    vec <- VSM.new (dim prob x)
    writeToMVector prob x vec
    return vec

  -- | Write a value of @'Domain' prob@ to a storable 'VSM.MVector'.
  -- 
  -- It can be thought as a variant of 'toVector' in destination-passing style.
  --
  -- @since 0.2.0.0
  writeToMVector :: PrimMonad m => prob -> Domain prob -> VSM.MVector (PrimState m) Double -> m ()
  writeToMVector prob x ret = VG.imapM_ (VGM.write ret) (toVector prob x)

  -- | Convert a storable 'Vector' back to a value of @'Domain' prob@
  --
  -- The @'Domain' prob@ argument is used as the return value's /shape/.
  --
  -- @since 0.2.0.0
  updateFromVector :: prob -> Domain prob -> Vector Double -> Domain prob

  -- | Objective function
  --
  -- It is called @fun@ in @scipy.optimize.minimize@.
  func :: prob -> Domain prob -> Double

  -- | Bounds
  --
  bounds :: prob -> Maybe (V.Vector (Double, Double))
  bounds _ = Nothing

  -- | Constraints
  constraints :: prob -> [Constraint]
  constraints _ = []

  {-# MINIMAL (toVector | dim, writeToMVector), updateFromVector, func #-}


-- | Optimization problem equipped with gradient information
class IsProblem prob => HasGrad prob where
  -- | Gradient of a function computed by 'func'
  --
  -- It is called @jac@ in @scipy.optimize.minimize@.
  grad :: prob -> Domain prob -> Domain prob
  grad prob = snd . grad' prob

  -- | Pair of 'func' and 'grad'
  grad' :: prob -> Domain prob -> (Double, Domain prob)
  grad' prob x = runST $ do
    gret <- VGM.new (dim prob x)
    y <- grad'M prob x gret
    g <- VG.unsafeFreeze gret
    return (y, updateFromVector prob x g)

  -- | Similar to 'grad'' but destination passing style is used for gradient vector
  grad'M :: PrimMonad m => prob -> Domain prob -> VSM.MVector (PrimState m) Double -> m Double
  grad'M prob x gvec = do
    let y = func prob x
    writeToMVector prob (grad prob x) gvec
    return y

  {-# MINIMAL grad | grad' | grad'M #-}


-- | Optimization problem equipped with hessian information
class IsProblem prob => HasHessian prob where
  -- | Hessian of a function computed by 'func'
  --
  -- It is called @hess@ in @scipy.optimize.minimize@.
  hessian :: prob -> Domain prob -> Matrix Double

  -- | The product of the hessian @H@ of a function @f@ at @x@ with a vector @x@.
  --
  -- It is called @hessp@ in @scipy.optimize.minimize@.
  --
  -- See also <https://hackage.haskell.org/package/ad-4.5.4/docs/Numeric-AD.html#v:hessianProduct>.
  hessianProduct :: prob -> Domain prob -> Domain prob -> Domain prob
  hessianProduct prob x v = updateFromVector prob x $ hessian prob x LA.#> toVector prob v

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
boundsUnconstrained n = V.replicate n (-infinity, infinity)

-- | Whether all lower bounds are -∞ and all upper bounds are +∞.
isUnconstainedBounds :: V.Vector (Double, Double) -> Bool
isUnconstainedBounds = V.all p
  where
    p (lb, ub) = isInfinite lb && lb < 0 && isInfinite ub && ub > 0


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
  let x0' = toVector prob x0
  ret <- minimizeV method (contramap (updateFromVector prob x0) params) (AsVectorProblem prob x0) x0'
  return $ fmap (updateFromVector prob x0) ret

minimizeV
  :: forall prob. (IsProblem prob, Optionally (HasGrad prob), Optionally (HasHessian prob))
  => Method  -- ^ Numerical optimization algorithm to use
  -> Params (Vector Double) -- ^ Parameters for optimization algorithms. Use 'def' as a default.
  -> AsVectorProblem prob  -- ^ Optimization problem to solve
  -> Vector Double  -- ^ Initial value
  -> IO (Result (Vector Double))
#ifdef WITH_CG_DESCENT
minimizeV CGDescent =
  case optionalDict @(HasGrad prob) of
    Just Dict -> minimize_CGDescent
    Nothing -> \_ _ _ -> throwIO GradUnavailable
#endif
#ifdef WITH_LBFGS
minimizeV LBFGS =
  case optionalDict @(HasGrad prob) of
    Just Dict -> minimize_LBFGS
    Nothing -> \_ _ _ -> throwIO GradUnavailable
#endif
#ifdef WITH_LBFGSB
minimizeV LBFGSB =
  case optionalDict @(HasGrad prob) of
    Just Dict -> minimize_LBFGSB
    Nothing -> \_ _ _ -> throwIO GradUnavailable
#endif
minimizeV Newton =
  case optionalDict @(HasGrad prob) of
    Nothing -> \_ _ _ -> throwIO GradUnavailable
    Just Dict ->
      case optionalDict @(HasHessian prob) of
        Nothing -> \_ _ _ -> throwIO HessianUnavailable
        Just Dict -> minimize_Newton
minimizeV method = \_ _ _ -> throwIO (UnsupportedMethod method)


#ifdef WITH_CG_DESCENT

minimize_CGDescent :: HasGrad prob => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize_CGDescent _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "CGDescent does not support bounds")
minimize_CGDescent _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "CGDescent does not support constraints")
minimize_CGDescent params prob x0 = do
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


#ifdef WITH_LBFGS

minimize_LBFGS :: HasGrad prob => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize_LBFGS _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "LBFGS does not support bounds")
minimize_LBFGS _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "LBFGS does not support constraints")
minimize_LBFGS params prob x0 = do
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


#ifdef WITH_LBFGSB

minimize_LBFGSB :: HasGrad prob => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize_LBFGSB _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "LBFGSB does not support constraints")
minimize_LBFGSB params prob x0 = do
  funcEvalRef <- newIORef (0::Int)
  gradEvalRef <- newIORef (0::Int)

  let bounds' =
        case bounds prob of
          Nothing -> []
          Just vec -> map convertB (VG.toList vec)
      convertB (lb, ub) =
        ( if isInfinite lb && lb < 0
          then Nothing
          else Just lb
        , if isInfinite ub && ub > 0
          then Nothing
          else Just ub
        )
      func' x = unsafePerformIO $ do
        modifyIORef' funcEvalRef (+1)
        evaluate (func prob x)
      grad' x = unsafePerformIO $ do
        modifyIORef' gradEvalRef (+1)
        evaluate (grad prob x)

  let -- | @m@: The maximum number of variable metric corrections used
      -- to define the limited memory matrix. /Suggestion:/ @5@.
      m = fromMaybe 5 (paramsMaxCorrections params)

      -- | @factr@: Iteration stops when the relative change in function value
      -- is smaller than @factr*eps@, where @eps@ is a measure of machine precision
      -- generated by the Fortran code. @1e12@ is low accuracy, @1e7@ is moderate,
      -- and @1e1@ is extremely high. Must be @>=1@. /Suggestion:/ @1e7@.
      factr = fromMaybe 1e7 $ (/ epsilon) <$> (paramsFTol params <|> paramsTol params)

      -- ^ @pgtol@: Iteration stops when the largest component of the projected
      -- gradient is smaller than @pgtol@. Must be @>=0@. /Suggestion:/ @1e-5@.
      pgtol = fromMaybe 1e-5 $ paramsGTol params <|> paramsTol params

      -- | @'Just' steps@ means the minimization is aborted if it has not converged after
      -- @steps>0@ iterations. 'Nothing' signifies no limit.
      steps = paramsMaxIters params

  result <- evaluate $ LBFGSB.minimize m factr pgtol steps bounds' x0 func' grad'

  let x = LBFGSB.solution result
      (success, msg) =
         case LBFGSB.stopReason result of
           LBFGSB.Converged -> (True, "The solution converged.")
           LBFGSB.StepCount -> (False, "The number of steps exceeded the user's request.")
           LBFGSB.Other msg -> (False, msg)

  funcEvals <- readIORef funcEvalRef
  gradEvals <- readIORef gradEvalRef

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
        { totalIters = length (LBFGSB.backtrace result)
        , funcEvals = funcEvals
        , gradEvals = gradEvals
        , hessEvals = 0
        , hessianEvals = 0
        }
    }

#endif


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

-- ------------------------------------------------------------------------

instance ToVector a => IsProblem (a -> Double) where
  type Domain (a -> Double) = a
  dim _ = ToVector.dim
  updateFromVector _ = ToVector.updateFromVector
  toVector _ = ToVector.toVector
  writeToMVector _ = ToVector.writeToMVector

  func f = f

instance Optionally (HasGrad (a -> Double)) where
  optionalDict = Nothing

instance Optionally (HasHessian (a -> Double)) where
  optionalDict = Nothing

-- ------------------------------------------------------------------------

-- | Wrapper type for adding gradient function to a problem
data WithGrad prob = WithGrad prob (Domain prob -> Domain prob)

instance IsProblem prob => IsProblem (WithGrad prob) where
  type Domain (WithGrad prob) = Domain prob
  dim (WithGrad prob _g) = dim prob
  updateFromVector (WithGrad prob _g) x0 = updateFromVector prob x0
  toVector (WithGrad prob _g) = toVector prob
  writeToMVector (WithGrad prob _g) = writeToMVector prob

  func (WithGrad prob _g) = func prob
  bounds (WithGrad prob _g) = bounds prob
  constraints (WithGrad prob _g) = constraints prob

instance IsProblem prob => HasGrad (WithGrad prob) where
  grad (WithGrad _prob g) = g

instance HasHessian prob => HasHessian (WithGrad prob) where
  hessian (WithGrad prob _g) = hessian prob
  hessianProduct (WithGrad prob _g) = hessianProduct prob

instance IsProblem prob => Optionally (HasGrad (WithGrad prob)) where
  optionalDict = hasOptionalDict

instance Optionally (HasHessian prob) => Optionally (HasHessian (WithGrad prob)) where
  optionalDict =
    case optionalDict @(HasHessian prob) of
      Just Dict -> hasOptionalDict
      Nothing -> Nothing

-- ------------------------------------------------------------------------

-- | Wrapper type for adding hessian to a problem
data WithHessian prob = WithHessian prob (Domain prob -> Matrix Double)

instance IsProblem prob => IsProblem (WithHessian prob) where
  type Domain (WithHessian prob) = Domain prob
  dim (WithHessian prob _hess) = dim prob
  updateFromVector (WithHessian prob _hess) x0 = updateFromVector prob x0
  toVector (WithHessian prob _hess) = toVector prob
  writeToMVector (WithHessian prob _g) = writeToMVector prob

  func (WithHessian prob _hess) = func prob
  bounds (WithHessian prob _hess) = bounds prob
  constraints (WithHessian prob _hess) = constraints prob

instance HasGrad prob => HasGrad (WithHessian prob) where
  grad (WithHessian prob _) = grad prob

instance IsProblem prob => HasHessian (WithHessian prob) where
  hessian (WithHessian _prob hess) = hess

instance Optionally (HasGrad prob) => Optionally (HasGrad (WithHessian prob)) where
  optionalDict =
    case optionalDict @(HasGrad prob) of
      Just Dict -> hasOptionalDict
      Nothing -> Nothing

instance IsProblem prob => Optionally (HasHessian (WithHessian prob)) where
  optionalDict = hasOptionalDict

-- ------------------------------------------------------------------------

-- | Wrapper type for adding bounds to a problem
data WithBounds prob = WithBounds prob (V.Vector (Double, Double))

instance IsProblem prob => IsProblem (WithBounds prob) where
  type Domain (WithBounds prob) = Domain prob
  dim (WithBounds prob _bounds) = dim prob
  updateFromVector (WithBounds prob _bounds) x0 = updateFromVector prob x0
  toVector (WithBounds prob _bounds) = toVector prob
  writeToMVector (WithBounds prob _g) = writeToMVector prob

  func (WithBounds prob _bounds) = func prob
  bounds (WithBounds _prob bounds) = Just bounds
  constraints (WithBounds prob _bounds) = constraints prob

instance HasGrad prob => HasGrad (WithBounds prob) where
  grad (WithBounds prob _bounds) = grad prob
  grad' (WithBounds prob _bounds) = grad' prob
  grad'M (WithBounds prob _bounds) = grad'M prob

instance HasHessian prob => HasHessian (WithBounds prob) where
  hessian (WithBounds prob _bounds) = hessian prob
  hessianProduct (WithBounds prob _bounds) = hessianProduct prob

instance Optionally (HasGrad prob) => Optionally (HasGrad (WithBounds prob)) where
  optionalDict =
    case optionalDict @(HasGrad prob) of
      Just Dict -> hasOptionalDict
      Nothing -> Nothing

instance Optionally (HasHessian prob) => Optionally (HasHessian (WithBounds prob)) where
  optionalDict =
    case optionalDict @(HasHessian prob) of
      Just Dict -> hasOptionalDict
      Nothing -> Nothing

-- ------------------------------------------------------------------------

-- | Wrapper type for adding constraints to a problem
data WithConstraints prob = WithConstraints prob [Constraint]

instance IsProblem prob => IsProblem (WithConstraints prob) where
  type Domain (WithConstraints prob) = Domain prob
  dim (WithConstraints prob _constraints) = dim prob
  updateFromVector (WithConstraints prob _constraints) x0 = updateFromVector prob x0
  toVector (WithConstraints prob _constraints) = toVector prob
  writeToMVector (WithConstraints prob _g) = writeToMVector prob

  func (WithConstraints prob _constraints) = func prob
  bounds (WithConstraints prob _constraints) = bounds prob
  constraints (WithConstraints _prob constraints) = constraints

instance HasGrad prob => HasGrad (WithConstraints prob) where
  grad (WithConstraints prob _constraints) = grad prob
  grad' (WithConstraints prob _constraints) = grad' prob
  grad'M (WithConstraints prob _constraints) = grad'M prob

instance HasHessian prob => HasHessian (WithConstraints prob) where
  hessian (WithConstraints prob _constraints) = hessian prob
  hessianProduct (WithConstraints prob _constraints) = hessianProduct prob

instance Optionally (HasGrad prob) => Optionally (HasGrad (WithConstraints prob)) where
  optionalDict =
    case optionalDict @(HasGrad prob) of
      Just Dict -> hasOptionalDict
      Nothing -> Nothing

instance Optionally (HasHessian prob) => Optionally (HasHessian (WithConstraints prob)) where
  optionalDict =
    case optionalDict @(HasHessian prob) of
      Just Dict -> hasOptionalDict
      Nothing -> Nothing

-- ------------------------------------------------------------------------

data AsVectorProblem prob = AsVectorProblem prob (Domain prob)

instance IsProblem prob => IsProblem (AsVectorProblem prob) where
  type Domain (AsVectorProblem prob) = Vector Double
  dim (AsVectorProblem prob x0) _ = dim prob x0
  updateFromVector _ _ = id
  toVector _ = id
  -- default implementation of 'writeToMVector' is what we want

  func (AsVectorProblem prob x0) = func prob . updateFromVector prob x0
  bounds (AsVectorProblem prob _x0) = bounds prob
  constraints (AsVectorProblem prob _x0) = constraints prob

instance HasGrad prob => HasGrad (AsVectorProblem prob) where
  grad (AsVectorProblem prob x0) x = toVector prob $ grad prob (updateFromVector prob x0 x)
  grad' (AsVectorProblem prob x0) x =
    case grad' prob (updateFromVector prob x0 x) of
      (y, g) -> (y, toVector prob g)
  grad'M (AsVectorProblem prob x0) x ret = grad'M prob (updateFromVector prob x0 x) ret

instance HasHessian prob => HasHessian (AsVectorProblem prob) where
  hessian (AsVectorProblem prob x0) x = hessian prob (updateFromVector prob x0 x)
  hessianProduct (AsVectorProblem prob x0) x v = toVector prob $ hessianProduct prob (updateFromVector prob x0 x) (updateFromVector prob x0 v)

-- ------------------------------------------------------------------------
