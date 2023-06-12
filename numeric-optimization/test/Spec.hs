{-# LANGUAGE OverloadedLists #-}
import Test.Hspec

import Control.Monad
import Data.Vector.Storable (Vector)
import Numeric.Optimization
import IsClose


main :: IO ()
main = hspec $ do
  describe "minimize" $ do
    context "when given rosenbrock function to LBFGS" $
      it "returns the global optimum" $ do
        result <- minimize LBFGS def (WithGrad rosenbrock rosenbrock') [-3,-4]
        resultSuccess result `shouldBe` True
        assertAllClose (def :: Tol Double) (resultSolution result) [1,1]

    when (isSupportedMethod LBFGSB) $ do
      context "when given rosenbrock function to LBFGSB" $
        it "returns the global optimum" $ do
          result <- minimize LBFGSB def (WithBounds (WithGrad rosenbrock rosenbrock') [(-4,2), (-5,2)]) [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]


-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: Vector Double -> Double
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)

rosenbrock' :: Vector Double -> Vector Double
rosenbrock' [x,y] =
  [ 2 * (1 - x) * (-1) + 100 * 2 * (y - sq x) * (-2) * x
  , 100 * 2 * (y - sq x)
  ]

sq :: Floating a => a -> a
sq x = x ** 2
