{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
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
-----------------------------------------------------------------------------
module Numeric.Optimization.AD
  ( 
    minimize
  , Constraint (..)
  , Method (..)
  , Params (..)
  , Result (..)
  , Statistics (..)
  , OptimizationException (..)

  -- * Re-exports
  , Default (..)
  ) where


import Control.Monad.Primitive
import Data.Default.Class
import Data.Foldable (foldlM)
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Numeric.Optimization as Opt
import Numeric.Optimization hiding (minimize, Params (..), IsProblem (..))

#if MIN_VERSION_ad(4,0,0)
import Data.Reflection (Reifies)
import qualified Numeric.AD.Mode.Reverse as AD
import qualified Numeric.AD.Internal.Reverse as AD (Tape)
#else
import qualified Numeric.AD.Types as AD
#endif


data Params f
  = Params
  { callback :: Maybe (f Double -> IO Bool)
    -- ^ If callback returns @True@, the algorithm execution is terminated.
  }

instance Default (Params f) where
  def =
    Params
    { callback = Nothing
    }


data Problem f
  = Problem
#if MIN_VERSION_ad(4,0,0)
      (forall s. Reifies s AD.Tape => f (AD.Reverse s Double) -> AD.Reverse s Double)
#else
      (forall s. AD.Mode s => f (AD.AD s Double) -> AD.AD s Double)
#endif
      (V.Vector (Double, Double))
      [Constraint]
      Int
      (f Int)


instance Traversable f => Opt.IsProblem (Problem f) where
  func (Problem f _bounds _constraints _size template) x =
#if MIN_VERSION_ad(4,0,0)
    fst $ AD.grad' f (fromVector template x)
#else
    AD.lowerFU f (fromVector template x)
#endif

  grad (Problem func _bounds _constraints size template) =
    toVector size . AD.grad func . fromVector template

  grad'M (Problem f _bounds _constraints _size template) x gvec = do
    case AD.grad' f (fromVector template x) of
      (y, g) -> do
        writeToMVector g gvec
        return y

  hessian (Problem _func _bounds _constraints _size _template) = undefined

  hessianProduct (Problem _func _bounds _constraints _size _template) = undefined

  bounds (Problem _f bounds _constraints _size _template) = bounds

  constraints (Problem _f _bounds constraints _size _template) = constraints


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


minimize
  :: forall f. Traversable f
  => Method
  -> Params f
  -> (forall s. Reifies s AD.Tape => f (AD.Reverse s Double) -> AD.Reverse s Double)  -- ^ Function to be minimized.
  -> Maybe (f (Double, Double))  -- ^ Bounds
  -> [Constraint]  -- ^ Constraintsa
  -> f Double -- ^ Initial value
  -> IO (f Double, Result, Statistics)
minimize method params f bounds constraints x0 = do
  let size :: Int
      template :: f Int
      (size, template) = mapAccumL (\i _ -> i `seq` (i+1, i)) 0 x0

      bounds' :: V.Vector (Double, Double)
      bounds' =
        case bounds of
          Just bs -> toVector size bs
          Nothing -> VG.replicate size (-1/0, 1/0)

      prob = Problem f bounds' constraints size template
      params' =
        Opt.Params
        { Opt.callback = fmap (\cb -> cb . fromVector template) (callback params)
        }

  (x, result, stat) <- Opt.minimize method params' prob (toVector size x0)
  return (fromVector template x, result, stat)
