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

    describe "literal pattern" $ do
      it "matches itself" $
        matchGlob "string" "string" `shouldBe` True
      it "shouldn't match another string" $
        matchGlob "string" "string prime" `shouldBe` False

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

    describe "escape pattern" $ do
      it "behaves as a literal pattern" $
        matchGlob "\\*" "*" `shouldBe` True
      it "does not execute the corresponding nonliteral pattern" $
        matchGlob "\\*" "" || matchGlob "\\*" "\\" `shouldBe` False
      it "does not match itself" $
        matchGlob "\\*" "\\*" `shouldBe` False

    describe "range pattern" $ do
      it "matches any character in range" $
        matchGlob "[abc]" "a" && matchGlob "[abc]" "b" && matchGlob "[abc]" "c" `shouldBe` True
