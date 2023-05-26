{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Backprop
-- Copyright   :  (c) Masahiro Sakai 2023
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.Backprop
  ( minimize
  , Method (..)
  , Params (..)
  , Result (..)
  , Statistics (..)

  -- * Re-exports
  , Default (..)
  ) where


import Control.Monad.Primitive
import Control.Monad.ST
import Data.Default.Class
import Data.MonoTraversable
import Data.Primitive.MutVar
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import Numeric.Backprop
import qualified Numeric.Optimization as Opt
import Numeric.Optimization hiding (minimize, Params (..), IsProblem (..))


data Params a
  = Params
  { callback :: Maybe (a -> IO Bool)
    -- ^ If callback returns @True@, the algorithm execution is terminated.
  }

instance Default (Params f) where
  def =
    Params
    { callback = Nothing
    }


data Problem a
  = Problem
      (forall s. Reifies s W => BVar s a -> BVar s Double)
      (V.Vector (Double, Double))
      Int
      a


instance (MonoTraversable a, Element a ~ Double, Backprop a) => Opt.IsProblem (Problem a) where
  func (Problem f _bounds _size x0) x = evalBP f (fromVec x0 x)

  grad (Problem f _bounds size x0) x = toVec size $ gradBP f (fromVec x0 x)

  grad'M (Problem f _bounds _size x0) x gvec = do
    case backprop f (fromVec x0 x) of
      (y, g) -> do
        writeToVec g gvec
        return y

  hessian (Problem _func _bounds _size _template) = undefined

  hessianProduct (Problem _func _bounds _size _template) = undefined

  bounds (Problem _f bounds _size _template) = bounds


fromVec :: (MonoTraversable a, VG.Vector v (Element a)) => a -> v (Element a) -> a
fromVec x0 vec = runST $ do
  cnt <- newMutVar 0
  oforM x0 $ \_ -> do
    i <- readMutVar cnt
    writeMutVar cnt $! i+1
    return (vec VG.! i)


toVec :: (MonoTraversable a, VG.Vector v (Element a)) => Int -> a -> v (Element a)
toVec size x = VG.create $ do
  vec <- VGM.new size
  writeToVec x vec
  return vec


writeToVec :: (PrimMonad m, MonoTraversable a, VGM.MVector mv (Element a)) => a -> mv (PrimState m) (Element a) -> m ()
writeToVec x vec = do
  cnt <- newMutVar 0
  oforM_ x $ \v -> do
    i <- readMutVar cnt
    writeMutVar cnt $! i+1
    VGM.write vec i v
  return ()


minimize
  :: forall a. (MonoTraversable a, Backprop a, Element a ~ Double)
  => Method
  -> Params a
  -> (forall s. Reifies s W => BVar s a -> BVar s Double)  -- ^ Function to be minimized.
  -> Maybe [(Double, Double)]  -- ^ Bounds
  -> a -- ^ Initial value
  -> IO (a, Result, Statistics)
minimize method params f bounds x0 = do
  let size :: Int
      size = olength x0

      bounds' :: V.Vector (Double, Double)
      bounds' = 
        case bounds of
          Nothing -> VG.replicate size (-1/0, 1/0)
          Just bs -> VG.fromList bs

      prob :: Problem a
      prob = Problem f bounds' size x0

      params' :: Opt.Params
      params' =
        Opt.Params
        { Opt.callback = fmap (\cb -> cb . fromVec x0) (callback params)
        }

  (x, result, stat) <- Opt.minimize method params' prob (toVec size x0)
  return (fromVec x0 x, result, stat)
