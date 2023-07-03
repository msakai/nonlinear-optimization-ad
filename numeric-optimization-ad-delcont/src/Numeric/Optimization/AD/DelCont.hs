{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.AD.DelCont
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
module Numeric.Optimization.AD.DelCont
  (
  -- * Problem specification
    UsingDelCont (..)

  -- * Utilities and Re-exports
  , AD
  , AD'
  , auto
  ) where


import Control.Monad.Primitive
import Control.Monad.ST
import Data.Foldable (foldlM, toList)
import Data.STRef
import Data.Traversable
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Numeric.AD.DelCont (AD, AD', auto)
import qualified Numeric.AD.DelCont as DelCont
import Numeric.Optimization

-- ------------------------------------------------------------------------

-- | Type for defining function and its gradient using automatic differentiation
-- provided by "Numeric.AD.DelCont".
--
-- Example:
--
-- > import Numeric.Optimization
-- > import Numeric.Optimization.AD.DelCont
-- >
-- > main :: IO ()
-- > main = do
-- >   result <- minimize LBFGS def (UsingDelCont rosenbrock) [-3,-4]
-- >   print (resultSuccess result)  -- True
-- >   print (resultSolution result)  -- [0.999999999009131,0.9999999981094296]
-- >   print (resultValue result)  -- 1.8129771632403013e-18
-- >
-- > -- https://en.wikipedia.org/wiki/Rosenbrock_function
-- > rosenbrock :: Floating a => [a] -> a
-- > -- rosenbrock :: [AD' s Double] -> AD' s Double
-- > rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
-- >
-- > sq :: Floating a => a -> a
-- > sq x = x ** 2
data UsingDelCont f
  = UsingDelCont (forall s. f (AD' s Double) -> AD' s Double)

instance Traversable f => IsProblem (UsingDelCont f) where
  type Domain (UsingDelCont f) = f Double

  dim _ = length

  toVector _ = VG.fromList . toList

  writeToMVector _ = writeToMVector'

  updateFromVector _ = updateFromVector'

  func (UsingDelCont f) x = fst $ DelCont.grad f x

  bounds (UsingDelCont _f) = Nothing

  constraints (UsingDelCont _f) = []

instance Traversable f => HasGrad (UsingDelCont f) where
  grad (UsingDelCont f) x = snd $ DelCont.grad f x

  grad' (UsingDelCont f) x = DelCont.grad f x

  grad'M prob@(UsingDelCont f) x gvec =
    case DelCont.grad f x of
      (y, g) -> do
        writeToMVector' g gvec
        return y

instance Traversable f => Optionally (HasGrad (UsingDelCont f)) where
  optionalDict = hasOptionalDict

instance Optionally (HasHessian (UsingDelCont f)) where
  optionalDict = Nothing

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
