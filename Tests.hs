{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Arrows #-}

module Main(main) where

import Prelude hiding ((.), id)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.FRP.Wire

import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Arrow.Operations(ArrowCircuit, delay)

integrate :: (ArrowCircuit a) => a (b, b) b -> b -> a b b
integrate add base = proc x -> do rec prevTotal <- delay base -< curTotal
                                      curTotal <- add -< (prevTotal, x)
                                  id -< curTotal

differentiate :: (ArrowCircuit a) => a (b, b) b -> b -> a b b
differentiate sub base = proc x -> do prevValue <- delay base -< x
                                      sub -< (prevValue, x)

numIntegrate :: (ArrowCircuit a, Num n) => n -> a n n
numIntegrate dt = integrate (arr step) 0
  where step (p, x) = p + dt*x

numDifferentiate :: (ArrowCircuit a, Fractional n) => n -> a n n
numDifferentiate dt = differentiate (arr step) 0
  where step (prevValue, value) = (value - prevValue) * factor
        factor = recip dt

testWire :: (ArrowApply a, ArrowChoice a) => Wire a b c -> a [b] [c]
testWire w = proc xs -> case xs of
                          []     -> id -< []
                          (x:xs) -> do (y, w') <- stepWire w -< x
                                       ys <- testWire w' -<< xs
                                       id -< y:ys

main :: IO ()
main = hspec $ do
  describe "Wire" $ do
    it "respects identity" $ do
      testWire id [1, 2, 3] `shouldBe` [1, 2, 3]
    it "can numerically integrate" $ do
      testWire (numIntegrate 1) [1, 2, 3] `shouldBe` [1, 3, 6]
    it "can numerically differentiate" $ do
      testWire (numDifferentiate 1) [1, 2, 3] `shouldBe` [1, 1, 1]
    it "can implement arr" $ property $
      \f x -> let apped = apply f in
                (testWire (arr apped) (x :: [Integer])) `shouldBe`
                 (map apped x :: [Integer])

