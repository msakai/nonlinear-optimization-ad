{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  -- * Main function
    minimize

  -- * Problem specification
  , Constraint (..)

  -- * Algorithm selection
  , Method (..)
  , isSupportedMethod
  , Params (..)

  -- * Result
  , Result (..)
  , Statistics (..)
  , OptimizationException (..)

  -- * Utilities and Re-exports
  , Default (..)
  , auto
  , Reverse
  , Reifies
  , Tape
  ) where


import Control.Monad.Primitive
import Data.Default.Class
import Data.Foldable (foldlM)
import Data.Functor.Contravariant
import Data.Reflection (Reifies)
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Numeric.AD.Internal.Reverse (Tape)
import Numeric.AD.Mode.Reverse (Reverse, auto)
import qualified Numeric.AD.Mode.Reverse as AD
import qualified Numeric.Optimization as Opt
import Numeric.Optimization hiding (minimize, IsProblem (..))


data Problem f
  = Problem
      (forall s. Reifies s Tape => f (Reverse s Double) -> Reverse s Double)
      (Maybe (V.Vector (Double, Double)))
      [Constraint]
      Int
      (f Int)

instance Traversable f => Opt.IsProblem (Problem f) where
  func (Problem f _bounds _constraints _size template) x =
    fst $ AD.grad' f (fromVector template x)

  bounds (Problem _f bounds _constraints _size _template) = bounds

  constraints (Problem _f _bounds constraints _size _template) = constraints

instance Traversable f => Opt.HasGrad (Problem f) where
  grad (Problem func _bounds _constraints size template) =
    toVector size . AD.grad func . fromVector template

  grad'M (Problem f _bounds _constraints _size template) x gvec = do
    case AD.grad' f (fromVector template x) of
      (y, g) -> do
        writeToMVector g gvec
        return y

instance Traversable f => Opt.Optionally (Opt.HasGrad (Problem f)) where
  optionalDict = hasOptionalDict

instance Opt.Optionally (Opt.HasHessian (Problem f)) where
  optionalDict = Nothing


fromVector :: (Functor f, VG.Vector v a) => f Int -> v a -> f a
fromVector template x = fmap (x VG.!) template


toVector :: (Traversable f, VG.Vector v a) => Int -> f a -> v a
toVector size x = VG.create $ do
  vec <- VGM.new size
  writeToMVector x vec
  return vec


writeToMVector :: (PrimMonad m, VGM.MVector mv a, Traversable f) => f a -> mv (PrimState m) a -> m ()
writeToMVector x vec = do
  _ <- foldlM (\i v -> VGM.write vec i v >> return (i+1)) 0 x
  return ()


-- | Minimization of scalar function of one or more variables.
--
-- This is a wrapper of 'Opt.minimize' and use "Numeric.AD.Mode.Reverse" to compute gradient.
--
-- Example:
--
-- > {-# LANGUAGE FlexibleContexts #-}
-- > import Numeric.Optimization.AD
-- > 
-- > main :: IO ()
-- > main = do
-- >   (x, result, stat) <- minimize LBFGS def rosenbrock Nothing [] [-3,-4]
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
minimize
  :: forall f. Traversable f
  => Method  -- ^ Numerical optimization algorithm to use
  -> Params (f Double)  -- ^ Parameters for optimization algorithms. Use 'def' as a default.
  -> (forall s. Reifies s Tape => f (Reverse s Double) -> Reverse s Double)  -- ^ Function to be minimized.
  -> Maybe (f (Double, Double))  -- ^ Bounds
  -> [Constraint]  -- ^ Constraintsa
  -> f Double -- ^ Initial value
  -> IO (Result (f Double))
minimize method params f bounds constraints x0 = do
  let size :: Int
      template :: f Int
      (size, template) = mapAccumL (\i _ -> i `seq` (i+1, i)) 0 x0

      bounds' :: Maybe (V.Vector (Double, Double))
      bounds' = fmap (toVector size) bounds

      prob = Problem f bounds' constraints size template
      params' = contramap (fromVector template) params

  result <- Opt.minimize method params' prob (toVector size x0)
  return $ fmap (fromVector template) result
