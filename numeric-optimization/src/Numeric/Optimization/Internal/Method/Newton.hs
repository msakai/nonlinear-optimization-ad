{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Optimization.Internal.Method.Newton
-- Copyright   :  (c) Masahiro Sakai 2023-2024
-- License     :  BSD-style
--
-- Maintainer  :  masahiro.sakai@gmail.com
-- Stability   :  provisional
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Numeric.Optimization.Internal.Method.Newton
  ( isSupported
  , minimize
  ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Generic as VG
import qualified Numeric.LinearAlgebra as LA
import Numeric.Optimization.Internal.Base


isSupported :: Bool
isSupported = True


minimize :: (HasGrad prob, HasHessian prob) => Params (Vector Double) -> AsVectorProblem prob -> Vector Double -> IO (Result (Vector Double))
minimize _params prob _ | not (isNothing (bounds prob)) = throwIO (UnsupportedProblem "Newton does not support bounds")
minimize _params prob _ | not (null (constraints prob)) = throwIO (UnsupportedProblem "Newton does not support constraints")
minimize params prob x0 = do
  let tol = fromMaybe 1e-6 (paramsTol params)

      loop !x !y !g !h !iter = do
        shouldStop <- msum <$> sequence
          [ pure $ case paramsMaxIters params of
              Just maxIter | maxIter <= iter -> Just "maximum number of iterations reached"
              _ -> Nothing
          , case paramsCallback params of
              Nothing -> return Nothing
              Just callback -> do
                flag <- callback x
                return $ if flag then Just "The minimization process has been canceled." else Nothing
          ]
        case shouldStop of
          Just reason ->
            return $
              Result
              { resultSuccess = False
              , resultMessage = reason
              , resultSolution = x
              , resultValue = y
              , resultGrad = Just g
              , resultHessian = Just h
              , resultHessianInv = Nothing
              , resultStatistics =
                  Statistics
                  { totalIters = iter
                  , funcEvals = iter + 1
                  , gradEvals = iter + 1
                  , hessEvals = iter + 1
                  , hessianEvals = iter + 1
                  }
              }
          Nothing -> do
            let p = h LA.<\> g
                x' = VG.zipWith (-) x p
            if LA.norm_Inf (VG.zipWith (-) x' x) > tol then do
              let (y', g') = grad' prob x'
                  h' = hessian prob x'
              loop x' y' g' h' (iter + 1)
            else do
              return $
                Result
                { resultSuccess = True
                , resultMessage = "success"
                , resultSolution = x
                , resultValue = y
                , resultGrad = Just g
                , resultHessian = Just h
                , resultHessianInv = Nothing
                , resultStatistics =
                    Statistics
                    { totalIters = iter
                    , funcEvals = iter + 1
                    , gradEvals = iter + 1
                    , hessEvals = iter + 1
                    , hessianEvals = iter + 1
                    }
                }

  let (y0, g0) = grad' prob x0
      h0 = hessian prob x0
  loop x0 y0 g0 h0 0

