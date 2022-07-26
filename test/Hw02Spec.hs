module Hw02Spec (spec) where

import qualified Hw02 as SUT
import Test.Hspec


spec :: Spec
spec = do
  describe "parseMessage" $ do
    it "parse message correctly" $ do
      SUT.parseMessage "E 2 562 help help" `shouldBe` SUT.LogMessage (SUT.Error 2) 562 "help help"
      SUT.parseMessage "I 29 la la la" `shouldBe` SUT.LogMessage SUT.Info 29 "la la la"
      SUT.parseMessage "wrong format" `shouldBe` SUT.Unknown "wrong format"
