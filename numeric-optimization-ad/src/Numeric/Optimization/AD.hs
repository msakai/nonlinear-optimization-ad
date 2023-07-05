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
-----------------------------------------------------------------------------
module Numeric.Optimization.AD
  (
  -- * Problem specification
    UsingReverse (..)
  , UsingSparse (..)

  -- * Utilities and Re-exports
  , AD
  , auto
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
import Numeric.AD.Mode.Reverse (Reverse)
import qualified Numeric.AD.Mode.Reverse as Reverse
import Numeric.AD.Mode.Sparse (Sparse)
import qualified Numeric.AD.Mode.Sparse as Sparse
import qualified Numeric.LinearAlgebra as LA
import Numeric.Optimization

-- ------------------------------------------------------------------------

-- | Type for defining function and its gradient using automatic differentiation
-- provided by "Numeric.AD.Mode.Reverse".
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
-- Unlike 'UsingReverse', it can be used with methods that requires hessian (e.g. 'Newton').
--
-- Example:
--
-- > import Numeric.Optimization
-- > import Numeric.Optimization.AD
-- >
-- > main :: IO ()
-- > main = do
-- >   (x, result, stat) <- minimize Newton def (UsingSparse rosenbrock) [-3,-4]
-- >   print (resultSuccess result)  -- True
-- >   print (resultSolution result)  -- [0.9999999999999999,0.9999999999999998]
-- >   print (resultValue result)  -- 1.232595164407831e-32
-- >
-- > -- https://en.wikipedia.org/wiki/Rosenbrock_function
-- > rosenbrock :: Floating a => [a] -> a
-- > -- rosenbrock :: [AD s (Sparse Double)] -> AD s (Sparse Double)
-- > rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
-- >
-- > sq :: Floating a => a -> a
-- > sq x = x ** 2
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
