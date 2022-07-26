module Hw01Spec (spec) where

import qualified Hw01 as SUT
import Test.Hspec


spec :: Spec
spec = describe "execise for hw01" $ do 
  it "validate credit card number correctly" $ do 
    SUT.validate 12312312312123 `shouldBe` False
    SUT.validate 4012888888881881 `shouldBe` True
    SUT.validate 4012888888881882 `shouldBe` False
  