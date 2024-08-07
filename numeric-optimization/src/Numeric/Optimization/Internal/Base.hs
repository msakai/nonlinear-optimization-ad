{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization
-- Copyright   :  (c) Masahiro Sakai 2023-2024
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.Internal.Base
  (
  -- * Problem specification
    IsProblem (..)
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
  , AsVectorProblem (..)

  -- * Algorithm selection
  , Method (..)
  , Params (..)

  -- * Result
  , Result (..)
  , Statistics (..)
  , OptimizationException (..)

  -- * Utilities
  , Optionally (..)
  , hasOptionalDict
  ) where

import Control.Exception
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Constraint (Dict (..))
import Data.Default.Class
import Data.Functor.Contravariant
import Data.Proxy
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import Numeric.Limits
import Numeric.LinearAlgebra (Matrix)
import qualified Numeric.LinearAlgebra as LA
import Numeric.Optimization.Utils.ToVector (ToVector)
import qualified Numeric.Optimization.Utils.ToVector as ToVector


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


-- | Optimization problems
--
-- Laws that should be satisfied:
--
-- * @'VS.length' . 'toVector' proxy = 'dim' proxy@
--
-- * @'updateFromVector' proxy a ('toVector' proxy a) = a@
--
-- * @'updateFromVector' proxy ('updateFromVector' proxy a v1) v2 = 'updateFromVector' proxy a v2@
--
-- ==== Design note
--
-- Note that 'Domain'-manipulation functions ('dim', 'toVector',
-- 'writeToMVector', and 'updateFromVector') cannot be factored out to
-- a parent class like
--
-- @
-- class ToVector ('Domain' prob) => 'IsProblem' prob
-- @
--
-- because how values of the domain are converted to/from vectors
-- depends on the 'IsProblem' instance.
--
-- For example, @(1, 2) :: (Double, Double)@ is usually viewed as a
-- two-dimensional vector, while
-- [ad](https://hackage.haskell.org/package/ad) package uses
-- 'Traversable' instance of a functor @(a,)@ and considers
-- @(a, 2) :: (a, Double)@ as a one-dimensional vector (!), thus
-- ad-based problem definition need to use the latter view.
class IsProblem prob where
  -- | Type of input values and gradients
  --
  -- @since 0.2.0.0
  type Domain prob

  -- | Dimention of a @'Domain' prob@ value.
  --
  -- @since 0.2.0.0
  dim :: Proxy prob -> Domain prob -> Int
  dim prob x = VS.length $ toVector prob x

  -- | Convert a @'Domain' prob@ value to a storable 'Vector'.
  --
  -- @since 0.2.0.0
  toVector :: Proxy prob -> Domain prob -> Vector Double
  toVector prob x = VS.create $ do
    vec <- VSM.new (dim prob x)
    writeToMVector prob x vec
    return vec

  -- | Write a value of @'Domain' prob@ to a storable 'VSM.MVector'.
  -- 
  -- It can be thought as a variant of 'toVector' in destination-passing style.
  --
  -- @since 0.2.0.0
  writeToMVector :: PrimMonad m => Proxy prob -> Domain prob -> VSM.MVector (PrimState m) Double -> m ()
  writeToMVector prob x ret = VG.imapM_ (VGM.write ret) (toVector prob x)

  -- | Convert a storable 'Vector' back to a value of @'Domain' prob@
  --
  -- The @'Domain' prob@ argument is used as the return value's /shape/.
  --
  -- @since 0.2.0.0
  updateFromVector :: Proxy prob -> Domain prob -> Vector Double -> Domain prob

  -- | Objective function
  --
  -- It is called @fun@ in @scipy.optimize.minimize@.
  func :: prob -> Domain prob -> Double

  -- | Bounds
  --
  -- The bounds are specified as a pair of the lower and upper bounds
  -- of feasible region. For example, if the domain is 'VS.Vector' and
  -- the feasible region is a unit cube, the bounds are
  -- @Just ('VG.fromList' [0, 0, 0], 'VG.fromList' [1, 1 ,1])@.
  bounds :: prob -> Maybe (Domain prob, Domain prob)
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
    gret <- VGM.new (dim (Proxy :: Proxy prob) x)
    y <- grad'M prob x gret
    g <- VG.unsafeFreeze gret
    return (y, updateFromVector (Proxy :: Proxy prob) x g)

  -- | Similar to 'grad'' but destination passing style is used for gradient vector
  grad'M :: PrimMonad m => prob -> Domain prob -> VSM.MVector (PrimState m) Double -> m Double
  grad'M prob x gvec = do
    let y = func prob x
    writeToMVector (Proxy :: Proxy prob) (grad prob x) gvec
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
  hessianProduct prob x v = updateFromVector (Proxy :: Proxy prob) x $ hessian prob x LA.#> toVector (Proxy :: Proxy prob) v

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
boundsUnconstrained :: forall prob. IsProblem prob => Proxy prob -> Domain prob -> (Domain prob, Domain prob)
boundsUnconstrained prob x = (lb, ub)
  where
    v = toVector prob x
    lb = updateFromVector prob x $ VG.map (\_ -> -infinity) v
    ub = updateFromVector prob x $ VG.map (\_ ->  infinity) v

-- | Whether all lower bounds are -∞ and all upper bounds are +∞.
isUnconstainedBounds :: forall prob. IsProblem prob => Proxy prob -> (Domain prob, Domain prob) -> Bool
isUnconstainedBounds prob (lb, ub) =
  VG.all (\b -> isInfinite b && b < 0) (toVector prob lb) &&
  VG.all (\b -> isInfinite b && b > 0) (toVector prob ub)

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
  dim _ = dim (Proxy :: Proxy prob)
  updateFromVector _ x0 = updateFromVector (Proxy :: Proxy prob) x0
  toVector _ = toVector (Proxy :: Proxy prob)
  writeToMVector _ = writeToMVector (Proxy :: Proxy prob)

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
  dim _ = dim (Proxy :: Proxy prob)
  updateFromVector _ x0 = updateFromVector (Proxy :: Proxy prob) x0
  toVector _ = toVector (Proxy :: Proxy prob)
  writeToMVector _ = writeToMVector (Proxy :: Proxy prob)

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
data WithBounds prob = WithBounds prob (Domain prob, Domain prob)

instance IsProblem prob => IsProblem (WithBounds prob) where
  type Domain (WithBounds prob) = Domain prob
  dim _ = dim (Proxy :: Proxy prob)
  updateFromVector _ x0 = updateFromVector (Proxy :: Proxy prob) x0
  toVector _ = toVector (Proxy :: Proxy prob)
  writeToMVector _ = writeToMVector (Proxy :: Proxy prob)

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
  dim _ = dim (Proxy :: Proxy prob)
  updateFromVector _ x0 = updateFromVector (Proxy :: Proxy prob) x0
  toVector _ = toVector (Proxy :: Proxy prob)
  writeToMVector _ = writeToMVector (Proxy :: Proxy prob)

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
  dim _ = VS.length
  updateFromVector _ _ = id
  toVector _ = id
  -- default implementation of 'writeToMVector' is what we want

  func (AsVectorProblem prob x0) = func prob . updateFromVector (Proxy :: Proxy prob) x0
  bounds (AsVectorProblem prob _x0) =
    case bounds prob of
      Nothing -> Nothing
      Just (lb, ub) -> Just (toVector (Proxy :: Proxy prob) lb, toVector (Proxy :: Proxy prob) ub)
  constraints (AsVectorProblem prob _x0) = constraints prob

instance HasGrad prob => HasGrad (AsVectorProblem prob) where
  grad (AsVectorProblem prob x0) x = toVector (Proxy :: Proxy prob) $ grad prob (updateFromVector (Proxy :: Proxy prob) x0 x)
  grad' (AsVectorProblem prob x0) x =
    case grad' prob (updateFromVector (Proxy :: Proxy prob) x0 x) of
      (y, g) -> (y, toVector (Proxy :: Proxy prob) g)
  grad'M (AsVectorProblem prob x0) x ret = grad'M prob (updateFromVector (Proxy :: Proxy prob) x0 x) ret

instance HasHessian prob => HasHessian (AsVectorProblem prob) where
  hessian (AsVectorProblem prob x0) x = hessian prob (updateFromVector (Proxy :: Proxy prob) x0 x)
  hessianProduct (AsVectorProblem prob x0) x v = toVector (Proxy :: Proxy prob) $ hessianProduct prob (updateFromVector (Proxy :: Proxy prob) x0 x) (updateFromVector (Proxy :: Proxy prob) x0 v)

-- ------------------------------------------------------------------------
