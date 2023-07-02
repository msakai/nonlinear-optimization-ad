{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
import Test.Hspec

import Control.Monad
import Numeric.Optimization
import Numeric.Optimization.Backprop
import Lens.Micro
import AllClose


main :: IO ()
main = hspec $ do
  describe "minimize" $ do
    when (isSupportedMethod LBFGS) $ do
      context "when given rosenbrock function to LBFGS" $
        it "returns the global optimum" $ do
          result <- minimize LBFGS def (UsingBackprop rosenbrock) (-3,-4)
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)


-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: forall s. Reifies s W => BVar s (Double, Double) -> BVar s Double
rosenbrock t = sq (1 - x) + 100 * sq (y - sq x)
  where
    sq x = x ** 2
    x = t ^^. _1
    y = t ^^. _2
