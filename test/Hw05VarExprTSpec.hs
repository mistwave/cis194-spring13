module Hw05VarExprTSpec where

import qualified Hw05VarExprT as SUT
import Test.Hspec
import Hw05 (add, lit, mul)
import Hw05VarExprT (var)


spec :: Spec
spec = describe "Homework 5: expr with vars" $ do
    it "withVars can compute with variables" $ do
      (SUT.withVars [("x", 6)] $ add (lit 3) (var "x")) `shouldBe` Just 9
      (SUT.withVars [("x", 6)] $ add (lit 3) (var "y")) `shouldBe` Nothing
      (SUT.withVars [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))) `shouldBe` Just 54
