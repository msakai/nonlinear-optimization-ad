{-# LANGUAGE GADTs, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Numeric.Optimization.Algorithms.HagerZhang05.Backprop as CG
import Text.Printf
import qualified Data.ByteString.Lazy as BS
import qualified Data.Csv as Csv
import qualified Data.Vector as V

main :: IO ()
main = do
  s <- BS.readFile "samples/galton.csv"
  let Right rows = Csv.decode Csv.HasHeader s
      samples :: [(Double, Double)]
      samples = V.toList rows
      -- hypothesis
      h [theta0,theta1] x = theta0 + theta1 * x
      h' theta x = h (CG.sequenceVar theta) x
      -- cost function
      cost theta = mse [(CG.auto x, CG.auto y) | (x,y) <- samples] (h' theta)
      params   = CG.defaultParameters{ CG.printFinal = True, CG.printParams = True, CG.verbose = CG.Verbose }
      grad_tol = 0.0000001
  (theta@[theta0,theta1], _result, _stat) <- CG.optimize params grad_tol [0,0] cost
  printf "y = %f x + %f\n" theta1 theta0
  printf "MSE = %f\n" (mse samples (h theta))
  printf "R^2 = %f\n" (r2 samples (h theta))

-- mean squared error
mse :: Fractional y => [(x,y)] -> (x -> y) -> y
mse samples h = mean [(h x - y)^(2::Int) | (x,y) <- samples]

r2 :: Fractional y => [(x,y)] -> (x -> y) -> y
r2 samples h = 1 - mse samples h / sum [(y - ym)^(2::Int) | y <- ys]
  where
    ys = map snd samples
    ym = mean ys

mean :: Fractional x => [x] -> x
mean xs = sum [x | x <- xs] / fromIntegral (length xs)
