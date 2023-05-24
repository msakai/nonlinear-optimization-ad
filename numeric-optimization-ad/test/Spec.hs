import Test.Hspec

import Numeric.Optimization.AD
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
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)
  where
    sq x = x ** 2
