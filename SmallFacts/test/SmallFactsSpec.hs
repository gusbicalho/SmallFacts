{-# LANGUAGE BlockArguments #-}
module SmallFactsSpec where

import Test.Hspec
import SmallFacts

spec :: Spec
spec = do
  describe "test" do
    it "works" $
      (1 :: Int) `shouldBe` 1