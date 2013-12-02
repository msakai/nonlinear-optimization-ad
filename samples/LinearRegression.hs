module Main where

import Control.Monad
import qualified Data.Vector as V
import Numeric.AD
import qualified Numeric.Optimization.Algorithms.HagerZhang05.AD as CG
import Text.Printf
import qualified Text.CSV as CSV

main :: IO ()
main = do
  Right csv <- CSV.parseCSVFromFile "galton.csv"
  let samples :: [(Double, Double)]
      samples = [(read parent, read child) | [child,parent] <- tail csv]      
      -- hypothesis
      h [theta0,theta1] x = theta0 + theta1 * x
      -- cost function
      cost theta = mse [(realToFrac x, realToFrac y) | (x,y) <- samples] (h theta)
      params   = CG.defaultParameters{ CG.printFinal = True, CG.printParams = True, CG.verbose = CG.Verbose }
      grad_tol = 0.0000001
  (theta, result, stat) <- CG.optimize params grad_tol [0,0] cost
  print theta

-- mean squared error
mse :: Fractional y => [(x,y)] -> (x -> y) -> y
mse samples h = sum [(h x - y)^(2::Int) | (x,y) <- samples] / fromIntegral (length samples)
