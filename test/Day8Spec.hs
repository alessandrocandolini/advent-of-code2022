{-# LANGUAGE QuasiQuotes #-}
module Day8Spec where

import Day8
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

