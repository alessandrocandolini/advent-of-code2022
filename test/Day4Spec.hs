{-# LANGUAGE QuasiQuotes #-}
module Day4Spec where

import Day4
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation

spec :: Spec
spec = describe "Day4" $ do

     it "" $
        1 `shouldBe` 1

     it "" $
        1 `shouldBe` 1

     it "" $
        1 `shouldBe` 1

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

