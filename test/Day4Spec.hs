{-# LANGUAGE QuasiQuotes #-}
module Day4Spec where

import Day4
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation

input =
  [trimming|
            2-4,6-8
            2-3,4-5
            5-7,7-9
            2-8,3-7
            6-6,4-6
            2-6,4-8
          |]

spec :: Spec
spec = describe "Day4" $ do

     it "count overlapping" $
        pureProgram input `shouldBe` (Report [Fully , Fully] [Partially, Fully, Fully, Partially])

     it "overlap single selection" $
        checkOverlap (Range 5 7) (Range 7 9) `shouldBe` Partially

     it "do not ovelap" $
        checkOverlap (Range 2 4) (Range 6 8) `shouldBe` No


     it "do not ovelap" $
        checkOverlap (Range 2 3) (Range 4 5) `shouldBe` No

     it "" $
        1 `shouldBe` 1

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

