{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IsClose
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

  -- * HUnit
  , assertAllClose
  ) where

import Data.Default.Class
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Semigroup
import GHC.Stack (HasCallStack)
import Test.HUnit
import Text.Printf

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

-- ------------------------------------------------------------------------

-- | Assert that two objects are equal up to desired tolerance.
assertAllClose
  :: (HasCallStack, AllClose r a, Show r, Show a)
  => Tol r
  -> a -- ^ actual
  -> a -- ^ desired
  -> Assertion
assertAllClose tol a b =
  case getAp (allCloseRaw tol a b) of
    Nothing ->
      assertString $ unlines $ header ++ ["x and y nan location mismatch:"] ++ footer
    Just (Sum numMismatch, Sum numTotal, Max absDiff, Max relDiff)
      | numMismatch == 0 -> return ()
      | otherwise ->
          assertString $ unlines $
            header ++
            [ printf "Mismatched elements: %d / %d (%f%%)" numMismatch numTotal (fromIntegral numMismatch * 100 / fromIntegral numTotal :: Double)
            , " Max absolute difference: " ++ show absDiff
            , " Max relative difference: " ++ show relDiff
            ] ++ footer
   where
     header, footer :: [String]
     header = [printf "Not equal to tolerance rtol=%s, atol=%s" (show (rtol tol)) (show (atol tol)), ""]
     footer = [" x: " ++ show a, " y: " ++ show b]

-- ------------------------------------------------------------------------
