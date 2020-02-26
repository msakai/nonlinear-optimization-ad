{-# LANGUAGE ScopedTypeVariables, Rank2Types, FlexibleContexts, CPP, TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
-- 
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Algorithms.HagerZhang05.Backprop
-- Copyright   :  (c) Masahiro Sakai 2020
-- License     :  GPL
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This package enhance
-- [nonlinear-optimization](https://hackage.haskell.org/package/nonlinear-optimization)'s
-- usability by using
-- [ad](https://hackage.haskell.org/package/nonlinear-optimization)'s
-- automatic differentiaion.  You only need to specify a function to
-- minimize and don't need to specify its gradient explicitly.
-----------------------------------------------------------------------------
module Numeric.Optimization.Algorithms.HagerZhang05.Backprop
  ( -- * Main function
    optimize
    -- * Result and statistics
  , Result(..)
  , Statistics(..)
    -- * Options
  , defaultParameters
  , Parameters(..)
  , Verbose(..)
  , LineSearch(..)
  , StopRules(..)
  , EstimateError(..)
    -- * Technical parameters
  , TechParameters(..)
    -- * Re-export
  , module Numeric.Backprop
  ) where

import Prelude hiding (mapM)
import Control.Monad.Primitive
import Control.Monad.State.Strict
import Data.MonoTraversable
import Data.Primitive.MutVar
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Numeric.Backprop
import Numeric.Optimization.Algorithms.HagerZhang05 hiding (optimize)
import qualified Numeric.Optimization.Algorithms.HagerZhang05 as HagerZhang05

{-# INLINE optimize #-}
-- | Run the @CG_DESCENT@ optimizer and try to minimize the function.
-- 
-- It uses reverse mode automatic differentiation to compute the gradient.
optimize
  :: forall a. (MonoTraversable a, Backprop a, Element a ~ Double)
  => Parameters  -- ^ How should we optimize.
  -> Double      -- ^ @grad_tol@, see 'stopRules'.
  -> a           -- ^ Initial guess.
  -> (forall s. Reifies s W => BVar s a -> BVar s Double) -- ^ Function to be minimized.
  -> IO (a, Result, Statistics)
optimize params grad_tol initial f = do
  let size :: Int
      size = olength initial

      readFromMVec :: PrimMonad m => SM.MVector (PrimState m) Double -> m a
      readFromMVec mx  = do
        cnt <- newMutVar 0
        oforM initial $ \_ -> do
          i <- readMutVar cnt
          writeMutVar cnt $! i+1
          SM.read mx i

      writeToMVec :: PrimMonad m => a -> SM.MVector (PrimState m) Double -> m ()
      writeToMVec x mx = do
        cnt <- newMutVar 0
        oforM_ x $ \v -> do
          i <- readMutVar cnt
          writeMutVar cnt $! i+1
          SM.write mx i v
        return ()

      readFromVec :: S.Vector Double -> a
      readFromVec x = flip evalState 0 $  do
        oforM initial $ \_ -> do
          i <- get
          put $ i+1
          return $! x S.! i

      mf :: forall m. PrimMonad m => PointMVector m -> m Double
      mf mx = do
        x <- readFromMVec mx
        return $ evalBP f x

      mg :: forall m. PrimMonad m => PointMVector m -> GradientMVector m -> m ()
      mg mx mret = do
        x <- readFromMVec mx
        writeToMVec (gradBP f x) mret

      mc :: (forall m. PrimMonad m => PointMVector m -> GradientMVector m -> m Double)
      mc mx mret = do
        x <- readFromMVec mx
        let (y,g) = backprop f x
        writeToMVec g mret
        return y

      vx0 :: S.Vector Double
      vx0 = S.create $ do
        mx <- SM.new size
        writeToMVec initial mx
        return mx

  (vx, result, stat) <- HagerZhang05.optimize params grad_tol vx0 (MFunction mf) (MGradient mg) (Just (MCombined mc))
  return (readFromVec vx, result, stat)
