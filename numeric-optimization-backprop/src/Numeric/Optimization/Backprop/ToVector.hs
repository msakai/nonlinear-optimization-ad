{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Backprop.ToVector
-- Copyright   :  (c) Masahiro Sakai 2023
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.Backprop.ToVector
  (
  -- * ToVector class
    ToVector (..)
  , toVector

  -- * Utilities for defining ToVector class

  -- *+ Foc Generics
  , ToVector' (..)

  -- ** Traversable-based definition
  , dimTraversable
  , writeToMVectorTraversable
  , updateFromVectorTraversable

  -- ** MonoTraversable-based definition
  , dimMonoTraversable
  , writeToMVectorMonoTraversable
  , updateFromVectorMonoTraversable
  ) where

import Control.Monad.Primitive
import Control.Monad.State
import qualified Data.MonoTraversable as MT
import Data.Traversable (mapAccumL)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as VSM
import qualified Data.Vector.Unboxed as VU
import GHC.Generics

import qualified Data.Functor.Identity as Functor
import qualified Data.Functor.Compose as Functor
import qualified Data.Functor.Const as Functor
import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor
import Data.IntMap (IntMap)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Monoid
import qualified Data.Semigroup as SG
import Data.Sequence (Seq)
import Data.Void

-- ------------------------------------------------------------------------

class ToVector a where
  dim :: a -> Int
  writeToMVector :: PrimMonad m => a -> VSM.MVector (PrimState m) Double -> m ()
  updateFromVector :: a -> VS.Vector Double -> a

  default dim :: (Generic a, ToVector' (Rep a)) => a -> Int
  dim x = dim' (from x)

  default writeToMVector :: (Generic a, ToVector' (Rep a), PrimMonad m) => a -> VSM.MVector (PrimState m) Double -> m ()
  writeToMVector x vec = writeToMVector' (from x) vec

  default updateFromVector :: (Generic a, ToVector' (Rep a)) => a -> VS.Vector Double -> a
  updateFromVector x v = to (updateFromVector' (from x) v)

toVector :: ToVector a => a -> VS.Vector Double
toVector x = VS.create $ do
  vec <- VSM.new (dim x)
  writeToMVector x vec
  return vec

-- ------------------------------------------------------------------------

dimTraversable :: Traversable f => f a -> Int
dimTraversable = length

writeToMVectorTraversable :: (Traversable f, ToVector a, PrimMonad m) => f a -> VSM.MVector (PrimState m) Double -> m ()
writeToMVectorTraversable xs vec = foldM_ f vec xs
  where
    f vec' x =
      case VSM.splitAt (dim x) vec' of
        (vec1, vec2) -> do
          writeToMVector x vec1
          return vec2

updateFromVectorTraversable :: (Traversable f, ToVector a) => f a -> VS.Vector Double -> f a
updateFromVectorTraversable xs v0 = flip evalState v0 $ do
  forM xs $ \x -> do
    v <- get
    case VS.splitAt (dim x) v of
      (v1, v2) -> do
        put v2
        return (updateFromVector x v1)

-- ------------------------------------------------------------------------

dimMonoTraversable :: MT.MonoTraversable a => a -> Int
dimMonoTraversable = MT.olength

writeToMVectorMonoTraversable :: (MT.MonoTraversable a, ToVector (MT.Element a), PrimMonad m) => a -> VSM.MVector (PrimState m) Double -> m ()
writeToMVectorMonoTraversable xs vec = MT.ofoldM f vec xs >> return ()
  where
    f vec' x =
      case VSM.splitAt (dim x) vec' of
        (vec1, vec2) -> do
          writeToMVector x vec1
          return vec2

updateFromVectorMonoTraversable :: (MT.MonoTraversable a, ToVector (MT.Element a)) => a -> VS.Vector Double -> a
updateFromVectorMonoTraversable xs v0 = flip evalState v0 $ do
  MT.oforM xs $ \x -> do
    v <- get
    case VS.splitAt (dim x) v of
      (v1, v2) -> do
        put v2
        return (updateFromVector x v1)

-- ------------------------------------------------------------------------

class ToVector' f where
  dim' :: f p -> Int
  writeToMVector' :: PrimMonad m => f p -> VSM.MVector (PrimState m) Double -> m ()
  updateFromVector' :: f p -> VS.Vector Double -> f p

instance ToVector' V1 where
  dim' x = case x of { }
  writeToMVector' _x _vec = return ()
  updateFromVector' x _v = case x of { }

instance ToVector' U1 where
  dim' _ = 0
  writeToMVector' _x _vec = return ()
  updateFromVector' x _v = x

instance (ToVector' f, ToVector' g) => ToVector' (f :+: g) where
  dim' (L1 x) = dim' x
  dim' (R1 x) = dim' x
  writeToMVector' (L1 x) vec = writeToMVector' x vec
  writeToMVector' (R1 x) vec = writeToMVector' x vec
  updateFromVector' (L1 x) v = L1 (updateFromVector' x v)
  updateFromVector' (R1 x) v = R1 (updateFromVector' x v)

instance (ToVector' f, ToVector' g) => ToVector' (f :*: g) where
  dim' (a :*: b) = dim' a + dim' b
  writeToMVector' (a :*: b) vec =
    case VSM.splitAt (dim' a) vec of
      (vec1, vec2) -> do
        writeToMVector' a vec1
        writeToMVector' b vec2
  updateFromVector' (a :*: b) v =
    case VS.splitAt (dim' a) v of
      (vec1, vec2) -> (updateFromVector' a vec1 :*: updateFromVector' b vec2)

instance (ToVector c) => ToVector' (K1 i c) where
  dim' (K1 x) = dim x
  writeToMVector' (K1 x) vec = writeToMVector x vec
  updateFromVector' (K1 x) v = K1 (updateFromVector x v)

instance (ToVector' f) => ToVector' (M1 i t f) where
  dim' (M1 x) = dim' x
  writeToMVector' (M1 x) vec = writeToMVector' x vec
  updateFromVector' (M1 x) v = M1 (updateFromVector' x v)

-- ------------------------------------------------------------------------

instance ToVector Double where
  dim _ = 1
  writeToMVector x vec = VSM.write vec 0 x
  updateFromVector _x v = v VS.! 0

instance (a ~ Double) => ToVector (VS.Vector a) where
  dim x = VS.length x
#if MIN_VERSION_vector(0,12,2)
  writeToMVector x vec = VS.imapM_ (VSM.write vec) x
#else
  writeToMVector x vec = flip evalStateT 0 $ VS.mapM_ (\e -> do{ i <- get; VSM.write vec i e; put (i+1) }) x
#endif
  updateFromVector _x v = v

instance (a ~ Double) => ToVector (VU.Vector a) where
  dim x = VU.length x
#if MIN_VERSION_vector(0,12,2)
  writeToMVector x vec = VU.imapM_ (VSM.write vec) x
#else
  writeToMVector x vec = flip evalStateT 0 $ VU.mapM_ (\e -> do{ i <- get; VSM.write vec i e; put (i+1) }) x
#endif
  updateFromVector _x v = VG.convert v

instance (ToVector a) => ToVector (V.Vector a) where
  dim xs = V.sum (V.map dim xs)
  writeToMVector xs vec = V.foldM_ f vec xs
    where
      f vec' x =
        case VSM.splitAt (dim x) vec' of
          (vec1, vec2) -> do
            writeToMVector x vec1
            return vec2
  updateFromVector xs v = snd $ mapAccumL f v xs
    where
      f v' x =
        case VS.splitAt (dim x) v' of
          (v1, v2) -> (v2, updateFromVector x v1)

instance ToVector Void

instance ToVector ()
instance (ToVector a, ToVector b) => ToVector (a, b)
instance (ToVector a, ToVector b, ToVector c) => ToVector (a, b, c)
instance (ToVector a, ToVector b, ToVector c, ToVector d) => ToVector (a, b, c, d)
instance (ToVector a, ToVector b, ToVector c, ToVector d, ToVector e) => ToVector (a, b, c, d, e)

instance (ToVector a) => ToVector (Maybe a)

instance ToVector a => ToVector (SG.Min a)
instance ToVector a => ToVector (SG.Max a)
instance ToVector a => ToVector (SG.First a)
instance ToVector a => ToVector (SG.Last a)
instance ToVector a => ToVector (SG.WrappedMonoid a)
instance ToVector a => ToVector (SG.Option a)
instance (ToVector a, ToVector b) => ToVector (SG.Arg a b)

instance ToVector a => ToVector (Dual a)
instance ToVector a => ToVector (Sum a)
instance ToVector a => ToVector (Product a)
instance ToVector a => ToVector (First a)
instance ToVector a => ToVector (Last a)
instance ToVector (f a) => ToVector (Alt f a)
instance ToVector (f a) => ToVector (Ap f a)

instance ToVector a => ToVector (Functor.Identity a)
instance ToVector (f (g a)) => ToVector (Functor.Compose f g a)
instance ToVector w => ToVector (Functor.Const w a)
instance (ToVector (f a), ToVector (g a)) => ToVector (Functor.Product f g a)
instance (ToVector (f a), ToVector (g a)) => ToVector (Functor.Sum f g a)

instance ToVector a => ToVector [a] where
  dim = dimTraversable
  writeToMVector = writeToMVectorTraversable
  updateFromVector = updateFromVectorTraversable

instance ToVector a => ToVector (NonEmpty a) where
  dim = dimTraversable
  writeToMVector = writeToMVectorTraversable
  updateFromVector = updateFromVectorTraversable

instance ToVector a => ToVector (Map k a) where
  dim = dimTraversable
  writeToMVector = writeToMVectorTraversable
  updateFromVector = updateFromVectorTraversable

instance ToVector a => ToVector (IntMap a) where
  dim = dimTraversable
  writeToMVector = writeToMVectorTraversable
  updateFromVector = updateFromVectorTraversable

instance ToVector a => ToVector (Seq a) where
  dim = dimTraversable
  writeToMVector = writeToMVectorTraversable
  updateFromVector = updateFromVectorTraversable

-- ------------------------------------------------------------------------
