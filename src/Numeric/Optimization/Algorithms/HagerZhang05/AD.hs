{-# LANGUAGE ScopedTypeVariables, Rank2Types, FlexibleContexts, CPP #-}
{-# OPTIONS_GHC -Wall #-}
module Numeric.Optimization.Algorithms.HagerZhang05.AD
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
  , Mode (..)
  ) where

import Prelude hiding (mapM)
import Control.Monad.Primitive
import Data.Foldable (foldlM)
import Data.Traversable (Traversable (..), mapAccumL, mapM)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import Numeric.AD
#if MIN_VERSION_ad(4,0,0)
import Data.Reflection (Reifies)
import Numeric.AD.Mode.Reverse
import Numeric.AD.Internal.Reverse (Tape)
#else
import Numeric.AD.Type
#endif
import Numeric.Optimization.Algorithms.HagerZhang05 hiding (optimize)
import qualified Numeric.Optimization.Algorithms.HagerZhang05 as HagerZhang05

{-# INLINE optimize #-}
-- | Run the @CG_DESCENT@ optimizer and try to minimize the function.
-- 
-- It uses reverse mode automatic differentiation to compute the gradient.
optimize
  :: forall f. Traversable f
  => Parameters  -- ^ How should we optimize.
  -> Double      -- ^ @grad_tol@, see 'stopRules'.
  -> f Double    -- ^ Initial guess.
#if MIN_VERSION_ad(4,0,0)
--  -> (forall s. (Mode s, Scalar s ~ Double) => f s -> s) -- ^ Function to be minimized.
  -> (forall s. Reifies s Tape => f (Reverse s Double) -> Reverse s Double) -- ^ Function to be minimized.
#else
  -> (forall s. Mode s => f (AD s Double) -> AD s Double) -- ^ Function to be minimized.
#endif
  -> IO (f Double, Result, Statistics)
optimize params grad_tol initial f = do
  let size :: Int
      template :: f Int
      (size, template) = mapAccumL (\i _ -> i `seq` (i+1, i)) 0 initial

      readFromMVec :: PrimMonad m => SM.MVector (PrimState m) Double -> m (f Double)
      readFromMVec mx  = mapM (SM.read mx) template

      writeToMVec :: PrimMonad m => f Double -> SM.MVector (PrimState m) Double -> m ()
      writeToMVec x mx = do
        _ <- foldlM (\i v -> SM.write mx i v >> return (i+1)) 0 x
        return ()

      readFromVec :: S.Vector Double -> f Double
      readFromVec x = fmap (x S.!) template

      mf :: forall m. (PrimMonad m, Functor m) => PointMVector m -> m Double
      mf mx = do
        x <- readFromMVec mx
#if MIN_VERSION_ad(4,0,0)
        return $ fst $ grad' f x
#else
        return $ lowerFU f x
#endif

      mg :: forall m. (PrimMonad m, Functor m) => PointMVector m -> GradientMVector m -> m ()
      mg mx mret = do
        x <- readFromMVec mx
        writeToMVec (grad f x) mret

      mc :: (forall m. (PrimMonad m, Functor m) => PointMVector m -> GradientMVector m -> m Double)
      mc mx mret = do
        x <- readFromMVec mx
        let (y,g) = grad' f x
        writeToMVec g mret
        return y

      vx0 :: S.Vector Double
      vx0 = S.create $ do
        mx <- SM.new size
        writeToMVec initial mx
        return mx

  (vx, result, stat) <- HagerZhang05.optimize params grad_tol vx0 (MFunction mf) (MGradient mg) (Just (MCombined mc))
  return (readFromVec vx, result, stat)
