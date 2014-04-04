module Main (main) where

import Test.Hspec

import Globber

main :: IO ()
main = hspec $ describe "Testing Globber" $ do

    describe "empty pattern" $ do
      it "matches empty string" $
        matchGlob "" "" `shouldBe` True
      it "shouldn't match non-empty string" $
        matchGlob "" "string" `shouldBe` False

    describe "wildcard pattern" $ do
      it "matches empty string" $
         matchGlob "*" "" `shouldBe` True
      it "matches arbitrary string" $
         matchGlob "*" "arbitrary string" `shouldBe` True

    describe "any character pattern" $ do
      it "does not match empty string" $
        matchGlob "?" "" `shouldBe` False
      it "matches one-character string" $
        matchGlob "?" "a" `shouldBe` True
      it "does not match multi-character string" $
        matchGlob "?" "aa" `shouldBe` False
