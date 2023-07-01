{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec

import Control.Exception
import Control.Monad
import Data.IORef
import Numeric.LinearAlgebra (Matrix, (><))
import Numeric.Optimization
import IsClose


main :: IO ()
main = hspec $ do
  describe "minimize CGDescent" $ do
    when (isSupportedMethod CGDescent) $ do
      it "can optimze scalar function" $ do
        let prob = (\(x :: Double) -> (1/4) * x**4 - x) `WithGrad` (\x -> x**3 - 1) `WithHessian` (\x -> (1 >< 1) [3 * x**2])
        result <- minimize CGDescent def prob 100
        resultSuccess result `shouldBe` True
        assertAllClose (def :: Tol Double) (resultSolution result) 1
        assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize CGDescent def prob (-3,-4)
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))
          case resultGrad result of
            Nothing -> return ()
            Just g -> assertAllClose (def :: Tol Double) g (grad prob (resultSolution result))
          resultHessian result `shouldBe` Nothing
          resultHessianInv result `shouldBe` Nothing
          let stat = resultStatistics result
          totalIters stat `shouldSatisfy` (> 0)
          funcEvals stat `shouldSatisfy` (> 0)
          gradEvals stat `shouldSatisfy` (> 0)
          hessianEvals stat `shouldBe` 0

      context "when given paramsMaxIters" $
        it "stops iterations early" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize CGDescent def{ paramsMaxIters = Just 2 } prob (1000, 1000)
          -- XXX: It seems that CG_DESCENT-C-3.0 report a number number of iterations that is 1 greater than the actual value
          totalIters (resultStatistics result) `shouldSatisfy` (\i -> 0 < i && i <= 2+1)
          resultSuccess result `shouldBe` False

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize CGDescent def rosenbrock (-3,-4) `shouldThrow` (GradUnavailable ==)

      context "when given a problem with bounds" $ do
        it "should throw UnsupportedProblem" $ do
          minimize CGDescent def (rosenbrock `WithGrad` rosenbrock' `WithBounds` [(-4,2), (-5,2)]) (-3,-4)
            `shouldThrow` (\case { UnsupportedProblem _ -> True; _ -> False })

  describe "minimize LBFGS" $ do
    when (isSupportedMethod LBFGS) $ do
      it "can optimze scalar function" $ do
        let prob = (\(x :: Double) -> (1/4) * x**4 - x) `WithGrad` (\x -> x**3 - 1) `WithHessian` (\x -> (1 >< 1) [3 * x**2])
        result <- minimize LBFGS def prob 100
        resultSuccess result `shouldBe` True
        assertAllClose (def :: Tol Double) (resultSolution result) 1
        assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize LBFGS def prob (-3,-4)
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))
          case resultGrad result of
            Nothing -> return ()
            Just g -> assertAllClose (def :: Tol Double) g (grad prob (resultSolution result))
          resultHessian result `shouldBe` Nothing
          resultHessianInv result `shouldBe` Nothing
          let stat = resultStatistics result
          totalIters stat `shouldSatisfy` (>0)
          funcEvals stat `shouldSatisfy` (>0)
          gradEvals stat `shouldSatisfy` (>0)
          hessianEvals stat `shouldBe` 0

      context "when given rosenbrock function with past" $
        it "returns the global optimum" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize LBFGS def{ paramsPast = Just 1 } prob (-3,-4)
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given callback" $
        it "stops iterations early" $ do
          let prob = rosenbrock `WithGrad` rosenbrock'
          counter <- newIORef (0 :: Int)
          let callback x = do
                evaluate x
                cnt <- readIORef counter
                writeIORef counter (cnt + 1)
                return (cnt >= 2)
          result <- minimize LBFGS def{ paramsCallback = Just callback } prob (1000, 1000)
          totalIters (resultStatistics result) `shouldBe` 3  -- ???
          resultSuccess result `shouldBe` False

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize LBFGS def rosenbrock (-3,-4) `shouldThrow` (GradUnavailable ==)

      context "when given a problem with bounds" $ do
        it "should throw UnsupportedProblem" $ do
          minimize LBFGS def (rosenbrock `WithGrad` rosenbrock' `WithBounds` [(-4,2), (-5,2)]) (-3,-4)
            `shouldThrow` (\case { UnsupportedProblem _ -> True; _ -> False })

  describe "minimize LBFGSB" $ do
    when (isSupportedMethod LBFGSB) $ do
      it "can optimze scalar function" $ do
        let prob = (\(x :: Double) -> (1/4) * x**4 - x) `WithGrad` (\x -> x**3 - 1) `WithHessian` (\x -> (1 >< 1) [3 * x**2])
        result <- minimize LBFGSB def prob 100
        resultSuccess result `shouldBe` True
        assertAllClose (def :: Tol Double) (resultSolution result) 1
        assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = rosenbrock `WithGrad` rosenbrock'
          result <- minimize LBFGSB def prob (-3,-4)
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))
          case resultGrad result of
            Nothing -> return ()
            Just g -> assertAllClose (def :: Tol Double) g (grad prob (resultSolution result))
          evaluate $ resultHessian result
          evaluate $ resultHessianInv result
          let stat = resultStatistics result
          totalIters stat `shouldSatisfy` (>0)
          funcEvals stat `shouldSatisfy` (>0)
          gradEvals stat `shouldSatisfy` (>0)
          hessianEvals stat `shouldBe` 0

      context "when given rosenbrock function with ftol (low accuracy)" $
        it "returns a solution not close enough to global optimum" $ do
          let prob = rosenbrock `WithGrad` rosenbrock'
              eps = 2.220446049250313e-16
          result <- minimize LBFGSB def{ paramsFTol = Just (1e12 * eps) } prob (-3,-4)
          resultSuccess result `shouldBe` True
          allClose (def :: Tol Double) (resultSolution result) (1,1) `shouldBe` False

      context "when given rosenbrock function with bounds" $
        it "returns the global optimum" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithBounds` [(-4,2), (-5,2)]
          result <- minimize LBFGSB def prob (-3,-4)
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)

      context "when given rosenbrock function with bounds (-infinity, +infinity)" $
        it "returns the global optimum" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithBounds` boundsUnconstrained 2
          result <- minimize LBFGSB def prob (-3,-4)
          resultSuccess result `shouldBe` True
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)

      context "when given paramsMaxIters" $
        it "stops iterations early" $ do
          let prob = WithGrad rosenbrock rosenbrock'
          result <- minimize LBFGSB def{ paramsMaxIters = Just 2 } prob (1000, 1000)
          totalIters (resultStatistics result) `shouldSatisfy` (\i -> 0 < i && i <= 2)
          resultSuccess result `shouldBe` False

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize LBFGSB def rosenbrock (-3,-4) `shouldThrow` (GradUnavailable ==)

  describe "minimize Newton" $ do
    when (isSupportedMethod Newton) $ do
      it "can optimze scalar function" $ do
        let prob = (\(x :: Double) -> (1/4) * x**4 - x) `WithGrad` (\x -> x**3 - 1) `WithHessian` (\x -> (1 >< 1) [3 * x**2])
        result <- minimize Newton def prob 100
        resultSuccess result `shouldBe` True
        assertAllClose (def :: Tol Double) (resultSolution result) 1
        assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))

      context "when given rosenbrock function" $
        it "returns the global optimum" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithHessian` rosenbrock''
          result <- minimize Newton def prob (-3,-4)
          resultSuccess result `shouldBe` True
          totalIters (resultStatistics result) `shouldSatisfy` (> 0)
          assertAllClose (def :: Tol Double) (resultSolution result) (1,1)
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))
          case resultGrad result of
            Nothing -> return ()
            Just g -> assertAllClose (def :: Tol Double) g (grad prob (resultSolution result))
          case resultHessian result of
            Nothing -> return ()
            Just h -> assertAllClose (def :: Tol Double) h (hessian prob (resultSolution result))
          let stat = resultStatistics result
          totalIters stat `shouldSatisfy` (>0)
          funcEvals stat `shouldSatisfy` (>0)
          gradEvals stat `shouldSatisfy` (>0)
          hessianEvals stat `shouldSatisfy` (>0)

      context "when given paramsMaxIters" $
        it "stops iterations early" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithHessian` rosenbrock''
          result <- minimize Newton def{ paramsMaxIters = Just 2 } prob (1000, 1000)
          totalIters (resultStatistics result) `shouldSatisfy` (\i -> 0 < i && i <= 2)
          resultSuccess result `shouldBe` False
          assertAllClose (def :: Tol Double) (resultValue result) (func prob (resultSolution result))
          case resultGrad result of
            Nothing -> return ()
            Just g -> assertAllClose (def :: Tol Double) g (grad prob (resultSolution result))
          case resultHessian result of
            Nothing -> return ()
            Just h -> assertAllClose (def :: Tol Double) h (hessian prob (resultSolution result))

      context "when given callback" $
        it "stops iterations early" $ do
          let prob = rosenbrock `WithGrad` rosenbrock' `WithHessian` rosenbrock''
          counter <- newIORef (0 :: Int)
          let callback x = do
                evaluate x
                cnt <- readIORef counter
                writeIORef counter (cnt + 1)
                return (cnt >= 2)
          result <- minimize Newton def{ paramsCallback = Just callback } prob (1000, 1000)
          resultSuccess result `shouldBe` False
          let stat = resultStatistics result
          totalIters stat `shouldBe` 2
          funcEvals stat `shouldSatisfy` (> 0)
          gradEvals stat `shouldSatisfy` (> 0)
          hessianEvals stat `shouldSatisfy` (> 0)

      context "when given a function without gradient" $ do
        it "should throw GradUnavailable" $ do
          minimize Newton def (rosenbrock `WithHessian` rosenbrock'') (-3,-4) `shouldThrow` (GradUnavailable ==)

      context "when given a function without Hessian" $ do
        it "should throw HessianUnavailable" $ do
          minimize Newton def (rosenbrock `WithGrad` rosenbrock') (-3,-4) `shouldThrow` (HessianUnavailable ==)

      context "when given a problem with bounds" $ do
        it "should throw UnsupportedProblem" $ do
          minimize Newton def (rosenbrock `WithGrad` rosenbrock' `WithHessian` rosenbrock'' `WithBounds` [(-4,2), (-5,2)]) (-3,-4)
            `shouldThrow` (\case { UnsupportedProblem _ -> True; _ -> False })

-- https://en.wikipedia.org/wiki/Rosenbrock_function
rosenbrock :: (Double, Double) -> Double
rosenbrock (x,y) = sq (1 - x) + 100 * sq (y - sq x)

rosenbrock' :: (Double, Double) -> (Double, Double)
rosenbrock' (x,y) =
  ( 2 * (1 - x) * (-1) + 100 * 2 * (y - sq x) * (-2) * x
  , 100 * 2 * (y - sq x)
  )

rosenbrock'' :: (Double, Double) -> Matrix Double
rosenbrock'' (x,y) =
  (2><2)
  [ 2 + 100 * 2 * (-2) * ((y - sq x) + (x * (-2) * x)), 100 * 2 * (-2) * x
  , 100 * 2 * (-2) * x, 100 * 2
  ]

sq :: Floating a => a -> a
sq x = x ** 2
