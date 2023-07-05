{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.AD
-- Copyright   :  (c) Masahiro Sakai 2023
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-- This module is a wrapper of "Numeric.Optimization" that uses
-- [ad](https://hackage.haskell.org/package/ad)'s automatic differentiation.
--
-- This module provides @Using/Foo/@ types for wrapping functions into
-- optimization problems ('IsProblem') that compute gradients (and
-- hessians) using automatic differentiation of ad's corresponding
-- @Numeric.AD.Mode./Foo/@ module.
--
-- Example:
--
-- > import Numeric.Optimization
-- > import Numeric.Optimization.AD
-- >
-- > main :: IO ()
-- > main = do
-- >   result <- minimize LBFGS def (UsingReverse rosenbrock) [-3,-4]
-- >   print (resultSuccess result)  -- True
-- >   print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
-- >   print (resultValue result)  -- 1.8129771632403013e-18
-- >
-- > -- https://en.wikipedia.org/wiki/Rosenbrock_function
-- > rosenbrock :: Floating a => [a] -> a
-- > -- rosenbrock :: Reifies s Tape => [Reverse s Double] -> Reverse s Double
-- > rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
-- >
-- > sq :: Floating a => a -> a
-- > sq x = x ** 2
--
-----------------------------------------------------------------------------
module Numeric.Optimization.AD
  (
  -- * Problem specification
    UsingDense (..)
  , UsingForward (..)
  , UsingKahn (..)
  , UsingReverse (..)
  , UsingSparse (..)

  -- * Utilities and Re-exports
  , AD
  , auto
  , Dense
  , Forward
  , Kahn
  , Reverse
  , Reifies
  , Tape
  , Sparse
  ) where


import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable (foldlM, toList)
import Data.Reflection (Reifies)
import Data.STRef
import Data.Traversable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Numeric.AD (AD, auto)
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Dense (Dense)
import qualified Numeric.AD.Mode.Dense as Dense
import Numeric.AD.Mode.Forward (Forward)
import qualified Numeric.AD.Mode.Forward as Forward
import Numeric.AD.Mode.Kahn (Kahn)
import qualified Numeric.AD.Mode.Kahn as Kahn
import Numeric.AD.Mode.Reverse (Reverse)
import qualified Numeric.AD.Mode.Reverse as Reverse
import Numeric.AD.Mode.Sparse (Sparse)
import qualified Numeric.AD.Mode.Sparse as Sparse
import qualified Numeric.LinearAlgebra as LA
import Numeric.Optimization

-- ------------------------------------------------------------------------

-- | Type for defining function and its gradient using automatic differentiation
-- provided by "Numeric.AD.Mode.Dense".
--
-- @since 0.2.0.0
data UsingDense f
  = UsingDense (forall s. f (AD s (Dense f Double)) -> AD s (Dense f Double))

instance Traversable f => IsProblem (UsingDense f) where
  type Domain (UsingDense f) = f Double

  dim _ = length

  toVector _ = VG.fromList . toList

  writeToMVector _ = writeToMVector'

  updateFromVector _ = updateFromVector'

  func (UsingDense f) x = fst $ Dense.grad' f x

  bounds (UsingDense _f) = Nothing

  constraints (UsingDense _f) = []

instance Traversable f => HasGrad (UsingDense f) where
  grad (UsingDense f) x = Dense.grad f x

  grad' (UsingDense f) x = Dense.grad' f x

  grad'M (UsingDense f) x gvec =
    case Dense.grad' f x of
      (y, g) -> do
        writeToMVector' g gvec
        return y

instance Traversable f => Optionally (HasGrad (UsingDense f)) where
  optionalDict = hasOptionalDict

instance Optionally (HasHessian (UsingDense f)) where
  optionalDict = Nothing

-- ------------------------------------------------------------------------

-- | Type for defining function and its gradient using automatic differentiation
-- provided by "Numeric.AD.Mode.Forward".
--
-- @since 0.2.0.0
data UsingForward f
  = UsingForward (forall s. f (AD s (Forward Double)) -> AD s (Forward Double))

instance Traversable f => IsProblem (UsingForward f) where
  type Domain (UsingForward f) = f Double

  dim _ = length

  toVector _ = VG.fromList . toList

  writeToMVector _ = writeToMVector'

  updateFromVector _ = updateFromVector'

  func (UsingForward f) x = fst $ Forward.grad' f x

  bounds (UsingForward _f) = Nothing

  constraints (UsingForward _f) = []

instance Traversable f => HasGrad (UsingForward f) where
  grad (UsingForward f) x = Forward.grad f x

  grad' (UsingForward f) x = Forward.grad' f x

  grad'M (UsingForward f) x gvec =
    case Forward.grad' f x of
      (y, g) -> do
        writeToMVector' g gvec
        return y

instance Traversable f => Optionally (HasGrad (UsingForward f)) where
  optionalDict = hasOptionalDict

instance Optionally (HasHessian (UsingForward f)) where
  optionalDict = Nothing

-- ------------------------------------------------------------------------

-- | Type for defining function and its gradient using automatic differentiation
-- provided by "Numeric.AD.Mode.Kahn".
--
-- @since 0.2.0.0
data UsingKahn f
  = UsingKahn (forall s. f (AD s (Kahn Double)) -> AD s (Kahn Double))

instance Traversable f => IsProblem (UsingKahn f) where
  type Domain (UsingKahn f) = f Double

  dim _ = length

  toVector _ = VG.fromList . toList

  writeToMVector _ = writeToMVector'

  updateFromVector _ = updateFromVector'

  func (UsingKahn f) x = fst $ Kahn.grad' f x

  bounds (UsingKahn _f) = Nothing

  constraints (UsingKahn _f) = []

instance Traversable f => HasGrad (UsingKahn f) where
  grad (UsingKahn f) x = Kahn.grad f x

  grad' (UsingKahn f) x = Kahn.grad' f x

  grad'M (UsingKahn f) x gvec =
    case Kahn.grad' f x of
      (y, g) -> do
        writeToMVector' g gvec
        return y

instance Traversable f => Optionally (HasGrad (UsingKahn f)) where
  optionalDict = hasOptionalDict

instance Optionally (HasHessian (UsingKahn f)) where
  optionalDict = Nothing

-- ------------------------------------------------------------------------

-- | Type for defining function and its gradient using automatic differentiation
-- provided by "Numeric.AD.Mode.Reverse".
--
-- @since 0.2.0.0
data UsingReverse f
  = UsingReverse (forall s. Reifies s Tape => f (Reverse s Double) -> Reverse s Double)

instance Traversable f => IsProblem (UsingReverse f) where
  type Domain (UsingReverse f) = f Double

  dim _ = length

  toVector _ = VG.fromList . toList

  writeToMVector _ = writeToMVector'

  updateFromVector _ = updateFromVector'

  func (UsingReverse f) x = fst $ Reverse.grad' f x

  bounds (UsingReverse _f) = Nothing

  constraints (UsingReverse _f) = []

instance Traversable f => HasGrad (UsingReverse f) where
  grad (UsingReverse f) x = Reverse.grad f x

  grad' (UsingReverse f) x = Reverse.grad' f x

  grad'M (UsingReverse f) x gvec =
    case Reverse.grad' f x of
      (y, g) -> do
        writeToMVector' g gvec
        return y

instance Traversable f => Optionally (HasGrad (UsingReverse f)) where
  optionalDict = hasOptionalDict

instance Optionally (HasHessian (UsingReverse f)) where
  optionalDict = Nothing

-- ------------------------------------------------------------------------

-- | Type for defining function and its gradient and hessian using automatic
-- differentiation provided by "Numeric.AD.Mode.Sparse".
--
-- It can be used with methods that requires hessian (e.g. 'Newton').
--
-- @since 0.2.0.0
data UsingSparse f
  = UsingSparse (forall s. f (AD s (Sparse Double)) -> AD s (Sparse Double))

instance Traversable f => IsProblem (UsingSparse f) where
  type Domain (UsingSparse f) = f Double

  dim _ = length

  toVector _ = VG.fromList . toList

  writeToMVector _ = writeToMVector'

  updateFromVector _ = updateFromVector'

  func (UsingSparse f) x = fst $ Sparse.grad' f x

  bounds (UsingSparse _f) = Nothing

  constraints (UsingSparse _f) = []

instance Traversable f => HasGrad (UsingSparse f) where
  grad (UsingSparse f) x = Sparse.grad f x

  grad' (UsingSparse f) x = Sparse.grad' f x

  grad'M (UsingSparse f) x gvec =
    case Sparse.grad' f x of
      (y, g) -> do
        writeToMVector' g gvec
        return y

instance Traversable f => HasHessian (UsingSparse f) where
  hessian (UsingSparse f) x = toMatrix (length x) $ Sparse.hessian f x
    where
      toMatrix n xss = (n LA.>< n) $ concat $ map toList $ toList xss

instance Traversable f => Optionally (HasGrad (UsingSparse f)) where
  optionalDict = hasOptionalDict

instance Traversable f => Optionally (HasHessian (UsingSparse f)) where
  optionalDict = hasOptionalDict

-- ------------------------------------------------------------------------

writeToMVector' :: (PrimMonad m, VGM.MVector mv a, Traversable f) => f a -> mv (PrimState m) a -> m ()
writeToMVector' x vec = do
  _ <- foldlM (\i v -> VGM.write vec i v >> return (i+1)) 0 x
  return ()

updateFromVector' :: (VG.Vector v a, Traversable f) => f a -> v a -> f a
updateFromVector' x0 vec = runST $ do
  counter <- newSTRef 0
  forM x0 $ \_ -> do
    i <- readSTRef counter
    writeSTRef counter $! i+1
    return $ vec VG.! i

-- ------------------------------------------------------------------------
