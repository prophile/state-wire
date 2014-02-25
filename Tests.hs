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
    it "can implement arr" $ property $
      \f x -> let apped = apply f in
                (testWire (arr apped) (x :: [Integer])) `shouldBe`
                 (map apped x :: [Integer])

