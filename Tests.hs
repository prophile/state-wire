{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Prelude hiding ((.), id)

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

import Control.FRP.Wire

import Control.Category
import Control.Arrow
import Control.Applicative
import Control.Arrow.Operations(delay)

main :: IO ()
main = hspec $ do
  describe "the test suite" $ do
    it "passes" $ do
      True `shouldBe` True
