{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Utils.AllClose
-- Copyright   :  (c) Masahiro Sakai 2023
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------
module Numeric.Optimization.Utils.AllClose
  (
  -- Tolerance type
    Tol (..)

  -- AllClose class
  , AllClose (..)
  , allCloseRawUnit
  , allCloseRawRealFrac
  , allCloseRawRealFloat

  -- * Re-exports
  , Default (..)
  ) where

import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Semigroup
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Numeric.LinearAlgebra as LA

-- ------------------------------------------------------------------------

-- | Tolerance
--
-- Values @a@ and @b@ are considered /close/ if @abs (a - b) <= atol + rtol * abs b@.
data Tol a
  = Tol
  { rtol :: a -- ^ The relative tolerance parameter (default: @1e-05@)
  , atol :: a -- ^ The absolute tolerance parameter (default: @1e-08@)
  , equalNan :: Bool -- ^ Whether to compare NaN’s as equal (default: @False@)
  } deriving (Show)

instance RealFrac a => Default (Tol a) where
  def = Tol
    { rtol = 1e-05
    , atol = 1e-08
    , equalNan = False
    }

-- ------------------------------------------------------------------------

class Real r => AllClose r a where
  -- | Returns number of mismatches, number of elements, maximal absolute difference, and maximal relative difference.
  -- Returns @'Ap' 'Nothing'@ if given values are incomparable.
  allCloseRaw :: Tol r -> a -> a -> Ap Maybe (Sum Int, Sum Int, Max r, Max r)

  -- | Returns 'True' if the two arrays are equal within the given tolerance; 'False' otherwise.
  allClose :: Tol r -> a -> a -> Bool
  allClose tol x y =
    case getAp (allCloseRaw tol x y) of
      Nothing -> False
      Just (Sum numMismatched, _, _, _) -> numMismatched == 0

allCloseRawRealFrac :: RealFrac r => Tol r -> r -> r -> Ap Maybe (Sum Int, Sum Int, Max r, Max r)
allCloseRawRealFrac t a b = Ap $ Just $
  ( Sum $ if abs (a - b) <= atol t + rtol t * abs b then 0 else 1
  , Sum 1
  , Max (abs (a - b))
  , Max (abs (a - b) / abs b)
  )

allCloseRawRealFloat :: RealFloat r => Tol r -> r -> r -> Ap Maybe (Sum Int, Sum Int, Max r, Max r)
allCloseRawRealFloat t a b
  | isNaN a /= isNaN b = Ap Nothing
  | otherwise = Ap $ Just $
      ( Sum $ if (equalNan t && isNaN a && isNaN b) || a == b || abs (a - b) <= atol t + rtol t * abs b then 0 else 1
      , Sum 1
      , Max (abs (a - b))
      , Max (abs (a - b) / abs b)
      )

allCloseRawUnit :: Num r => Ap Maybe (Sum Int, Sum Int, Max r, Max r)
allCloseRawUnit = Ap (Just (Sum 0, Sum 0, Max 0, Max 0))

instance AllClose Rational Rational where
  allCloseRaw = allCloseRawRealFrac

instance AllClose Double Double where
  allCloseRaw = allCloseRawRealFloat

instance (AllClose r a) => AllClose r (Maybe a) where
  allCloseRaw tol (Just a) (Just b) = allCloseRaw tol a b
  allCloseRaw _ Nothing Nothing = allCloseRawUnit
  allCloseRaw _ _ _ = Ap Nothing

instance (AllClose r v) => AllClose r [v] where
  allCloseRaw tol xs ys
    | length xs == length ys = sconcat (allCloseRawUnit :| [allCloseRaw tol a b | (a,b) <- zip xs ys])
    | otherwise = Ap Nothing

instance (Ord k, AllClose r v) => AllClose r (Map k v) where
  allCloseRaw tol m1 m2
    | Map.keys m1 == Map.keys m2 = sconcat (allCloseRawUnit :| [allCloseRaw tol a b | (a,b) <- zip (Map.elems m1) (Map.elems m2)])
    | otherwise = Ap Nothing

instance (AllClose r v) => AllClose r (V.Vector v) where
  allCloseRaw tol xs ys
    | VG.length xs == VG.length ys = sconcat (allCloseRawUnit :| [allCloseRaw tol a b | (a,b) <- zip (VG.toList xs) (VG.toList ys)])
    | otherwise = Ap Nothing

instance (AllClose r v, VS.Storable v) => AllClose r (VS.Vector v) where
  allCloseRaw tol xs ys
    | VG.length xs == VG.length ys = sconcat (allCloseRawUnit :| [allCloseRaw tol a b | (a,b) <- zip (VG.toList xs) (VG.toList ys)])
    | otherwise = Ap Nothing

instance (AllClose r v, VU.Unbox v) => AllClose r (VU.Vector v) where
  allCloseRaw tol xs ys
    | VG.length xs == VG.length ys = sconcat (allCloseRawUnit :| [allCloseRaw tol a b | (a,b) <- zip (VG.toList xs) (VG.toList ys)])
    | otherwise = Ap Nothing

instance (AllClose r v, Num v, LA.Container LA.Vector v) => AllClose r (LA.Matrix v) where
  allCloseRaw tol xs ys
    | LA.size xs == LA.size ys = allCloseRaw tol (LA.flatten xs) (LA.flatten ys)
    | otherwise = Ap Nothing

instance (AllClose r v1, AllClose r v2) => AllClose r (v1, v2)  where
  allCloseRaw tol (x1,y1) (x2,y2) = allCloseRaw tol x1 x2 <> allCloseRaw tol y1 y2

-- ------------------------------------------------------------------------
