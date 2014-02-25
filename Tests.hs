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
    it "can implement arr" $ property $
      \f x -> let apped = apply f in
                (testWire (arr apped) (x :: [Integer])) `shouldBe`
                 (map apped x :: [Integer])

