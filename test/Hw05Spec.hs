module Hw05Spec where

import qualified Hw05 as SUT
import ExprT
import Test.Hspec
import Hw05 (lit, add, mul)


spec :: Spec
spec = describe "Homework 5" $ do
  it "eval the ExprT should return right value" $ do
    SUT.eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` 20

  it "evalStr should return the right value" $ do
    SUT.evalStr "(2+3)*4" `shouldBe` Just 20
    SUT.evalStr "2+3*4" `shouldBe` Just 14
    SUT.evalStr "(2+3)*" `shouldBe` Nothing

  it "reify should make expr a ExprT" $ do
    (SUT.reify $ mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` (Mul (Add (Lit 2) (Lit 3)) (Lit 4))

  it "testExpr should return right values in every type" $ do
    (SUT.testExp :: Maybe Integer) `shouldBe` Just (-7)
    (SUT.testExp :: Maybe Bool) `shouldBe` Just True
    (SUT.testExp :: Maybe SUT.MinMax) `shouldBe` Just (SUT.MinMax 5)
    (SUT.testExp :: Maybe SUT.Mod7) `shouldBe` Just (SUT.Mod7 0)
