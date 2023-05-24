{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
import Test.Hspec

import Numeric.Backprop
import Numeric.Optimization.Backprop
import Lens.Micro
import IsClose


main :: IO ()
main = hspec $ do
  describe "minimize" $ do
    context "when given rosenbrock function" $
      it "returns the global optimum" $ do
        (x, result, stat) <- minimize LBFGS def rosenbrock Nothing [-3,-4]
        resultSuccess result `shouldBe` True
        assertAllClose (def :: Tol Double) x [1,1]


-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: forall s. Reifies s W => BVar s [Double] -> BVar s Double
rosenbrock t = sq (1 - x) + 100 * sq (y - sq x)
  where
    sq x = x ** 2
    Just x = t ^^? ix 0
    Just y = t ^^? ix 1
