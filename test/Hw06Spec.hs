module Hw06Spec where

import qualified Hw06 as SUT
import Test.Hspec


spec :: Spec
spec = describe "Homework 6" $ do
        it "all fibs should return right fibonacci sequence" $ do
          take 20 SUT.fibs1 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
          take 20 SUT.fibs2 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
          take 20 (SUT.streamToList SUT.fibs3) `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
          take 20 SUT.fibs4 `shouldBe` [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181]
