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

import Data.Monoid
import Data.Maybe

testWire :: (ArrowApply a, ArrowChoice a) => Wire a b c -> a [b] [c]
testWire w = proc xs -> case xs of
                          []     -> id -< []
                          (x:xs) -> do (y, w') <- stepWire w -< x
                                       ys <- testWire w' -<< xs
                                       id -< y:ys

latch :: (ArrowWire a) => a (Maybe b) (Maybe b)
latch = Last ^>> accumulate >>^ getLast

inhibit :: (ArrowWire a) => a (Maybe b) (Maybe b)
inhibit = First ^>> accumulate >>^ getFirst

lock :: (ArrowWire a) => a (c, Bool) (Maybe c)
lock = condition ^>> latch
  where condition ~(x, en) = if en then Just x else Nothing

main :: IO ()
main = hspec $ do
  describe "Wire" $ do
    it "respects identity" $ do
      testWire id [1, 2, 3] `shouldBe` [1, 2, 3]
    it "can latch" $ do
      testWire latch [Nothing, Just 2, Nothing, Just 3, Nothing] `shouldBe`
                     [Nothing, Just 2, Just 2, Just 3, Just 3]
    it "can inhibit" $ do
      testWire inhibit [Nothing, Just 2, Just 3, Nothing] `shouldBe`
                       [Nothing, Just 2, Just 2, Just 2]
    it "can lock, to a range" $ do
      let inRange x = (x >= 0) && (x <= 10)
      testWire ((id &&& arr inRange) >>> lock >>^ fromMaybe 0)
        [3, 6, -3, -5, 8, 10, 11] `shouldBe`
        [3, 6,  6,  6, 8, 10, 10]
    it "can implement arr" $ property $
      \f x -> let apped = apply f in
                (testWire (arr apped) (x :: [Integer])) `shouldBe`
                 (map apped x :: [Integer])

