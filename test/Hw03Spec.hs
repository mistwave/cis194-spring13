module Hw03Spec where

import qualified Hw03 as SUT

import Test.Hspec

spec :: Spec
spec = describe "Homework 3: code golf" $ do
  it "hopscotch works as expected" $ do
    SUT.skips "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
    SUT.skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
    SUT.skips [1] `shouldBe` [[1]]
    SUT.skips [True, False] `shouldBe` [[True, False], [False]]
    SUT.skips [] `shouldBe` ([] :: [[Int]])

  it "local maxima works as expected" $ do
    SUT.localMaxima [2,9,5,6,1] `shouldBe` [9,6]
    SUT.localMaxima [2,3,4,1,5] `shouldBe` [4]
    SUT.localMaxima [1..5] `shouldBe` []
