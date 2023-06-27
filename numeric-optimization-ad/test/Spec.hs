import Test.Hspec

import Control.Monad
import Numeric.Optimization
import Numeric.Optimization.AD
import IsClose


main :: IO ()
main = hspec $ do
  describe "minimizeReverse" $ do
    when (isSupportedMethod LBFGS) $ do
      context "when given rosenbrock function to LBFGS" $
        it "returns the global optimum" $ do
          result <- minimize LBFGS def (UsingReverse rosenbrock) [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]

  describe "minimizeSparse" $ do
    when (isSupportedMethod Newton) $ do
      context "when given rosenbrock function to Newton" $
        it "returns the global optimum" $ do
          result <- minimize Newton def (UsingSparse rosenbrock) [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]


-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
  where
    sq x = x ** 2
