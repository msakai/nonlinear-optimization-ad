{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module AllClose
  (
  -- * Re-exports
    module Numeric.Optimization.Utils.AllClose

  -- * HUnit
  , assertAllClose
  ) where

import Data.Default.Class
import Data.Monoid
import Data.Semigroup
import GHC.Stack (HasCallStack)
import Test.HUnit
import Text.Printf

import Numeric.Optimization.Utils.AllClose

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
