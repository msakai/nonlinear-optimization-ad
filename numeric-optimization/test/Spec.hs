{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
import Test.Hspec

import Control.Monad
import Data.Vector.Storable (Vector)
import Numeric.LinearAlgebra (Matrix, (><))
import Numeric.Optimization
import IsClose


main :: IO ()
main = hspec $ do
  describe "minimize CGDescent" $ do
    when (isSupportedMethod CGDescent) $ do
      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize CGDescent def prob [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given paramsMaxIterations" $
        it "stops iterations early" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize CGDescent def{ paramsMaxIterations = Just 2 } prob [1000, 1000]
          -- XXX: It seems that CG_DESCENT-C-3.0 report a number number of iterations that is 1 greater than the actual value
          totalIters (resultStatistics result) `shouldSatisfy` (<=2+1)
          resultSuccess result `shouldBe` False

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize CGDescent def rosenbrock [-3,-4] `shouldThrow` (GradUnavailable ==)

      context "when given a problem with bounds" $ do
        it "should throw UnsupportedProblem" $ do
          minimize CGDescent def (rosenbrock `WithGrad` rosenbrock' `WithBounds` [(-4,2), (-5,2)]) [-3,-4]
            `shouldThrow` (\case { UnsupportedProblem _ -> True; _ -> False })

  describe "minimize LBFGS" $ do
    when (isSupportedMethod LBFGS) $ do
      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize LBFGS def prob [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize LBFGS def rosenbrock [-3,-4] `shouldThrow` (GradUnavailable ==)

      context "when given a problem with bounds" $ do
        it "should throw UnsupportedProblem" $ do
          minimize LBFGS def (rosenbrock `WithGrad` rosenbrock' `WithBounds` [(-4,2), (-5,2)]) [-3,-4]
            `shouldThrow` (\case { UnsupportedProblem _ -> True; _ -> False })

  describe "minimize LBFGSB" $ do
    when (isSupportedMethod LBFGSB) $ do
      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithBounds` [(-4,2), (-5,2)]
          result <- minimize LBFGSB def prob [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given paramsMaxIterations" $
        it "stops iterations early" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize LBFGSB def{ paramsMaxIterations = Just 2 } prob [1000, 1000]
          totalIters (resultStatistics result) `shouldSatisfy` (<=2)
          resultSuccess result `shouldBe` False

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize LBFGSB def rosenbrock [-3,-4] `shouldThrow` (GradUnavailable ==)

  describe "minimize Newton" $ do
    when (isSupportedMethod Newton) $ do
      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithHessian` rosenbrock''
          result <- minimize Newton def prob [-3,-4]
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) [1,1]
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given paramsMaxIterations" $
        it "stops iterations early" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithHessian` rosenbrock''
          result <- minimize Newton def{ paramsMaxIterations = Just 2 } prob [1000, 1000]
          traceShowM result
          totalIters (resultStatistics result) `shouldSatisfy` (<=2)
          resultSuccess result `shouldBe` False

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize Newton def (rosenbrock `WithHessian` rosenbrock'') [-3,-4] `shouldThrow` (GradUnavailable ==)

      context "when given a function without Hessian" $ do
        it "should throw HessianUnavailable" $ do
          minimize Newton def (rosenbrock `WithGrad` rosenbrock') [-3,-4] `shouldThrow` (HessianUnavailable ==)

      context "when given a problem with bounds" $ do
        it "should throw UnsupportedProblem" $ do
          minimize Newton def (rosenbrock `WithGrad` rosenbrock' `WithHessian` rosenbrock'' `WithBounds` [(-4,2), (-5,2)]) [-3,-4]
            `shouldThrow` (\case { UnsupportedProblem _ -> True; _ -> False })

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: Vector Double -> Double
rosenbrock [x,y] = sq (1 - x) + 100 * sq (y - sq x)

rosenbrock' :: Vector Double -> Vector Double
rosenbrock' [x,y] =
  [ 2 * (1 - x) * (-1) + 100 * 2 * (y - sq x) * (-2) * x
  , 100 * 2 * (y - sq x)
  ]

rosenbrock'' :: Vector Double -> Matrix Double
rosenbrock'' [x,y] =
  (2><2)
  [ 2 + 100 * 2 * (-2) * ((y - sq x) + (x * (-2) * x)), 100 * 2 * (-2) * x
  , 100 * 2 * (-2) * x, 100 * 2
  ]

sq :: Floating a => a -> a
sq x = x ** 2
