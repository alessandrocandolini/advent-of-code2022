{-# LANGUAGE QuasiQuotes #-}
module Day10Spec where

import Day10
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation

input =
  [trimming|
          |]


spec :: Spec
spec = describe "Day N" $ do

     it "" $
        1 `shouldBe` 1

     prop "" $
        \l -> reverse ( reverse l ) == ( l::[Int])

