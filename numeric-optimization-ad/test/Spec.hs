import Test.Hspec

import Control.Monad
import Numeric.Optimization.AD
import IsClose


main :: IO ()
main = hspec $ do
  describe "minimize" $ do
    when (isSupportedMethod LBFGS) $ do
      context "when given rosenbrock function to LBFGS" $
        it "returns the global optimum" $ do
          result <- minimize LBFGS def rosenbrock Nothing [] [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]


-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
  where
    sq x = x ** 2
