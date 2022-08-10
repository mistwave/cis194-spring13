module Hw04Spec where

import qualified Hw04 as SUT
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = describe "Homework 4: wholemeal programming" $ do
  prop "fun1' behaves the same as fun1" $ \list ->
    SUT.fun1' list `shouldBe` SUT.fun1 list

  it "fun2' behaves the same as fun2 when x > 0" $ do
    map SUT.fun2' [1..100] `shouldBe` map SUT.fun2 [1..100]

  it "xor should return True iff. there and odd number of True" $ do
    SUT.xor [True, False, False] `shouldBe` True
    SUT.xor [False, True, False, False, True] `shouldBe` False

  it "map' behaves the same as map" $ do
    SUT.map' (*2) [1..10] `shouldBe` map (*2) [1..10]

  it "myFoldl behaves the same as foldl" $ do
    SUT.myFoldl (flip (:)) [] [1..9] `shouldBe` [9,8..1]

  it "sieveSundaram should get right primes" $ do
    SUT.sieveSundaram 0 `shouldBe` [2]
    SUT.sieveSundaram 1 `shouldBe` [2,3]
    SUT.sieveSundaram 10 `shouldBe` [2,3,5,7,11,13,17,19]
