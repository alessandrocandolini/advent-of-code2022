{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import qualified Data.Set as S
import Day4
import NeatInterpolation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Data.List
import Control.Monad

input =
  [trimming|
            2-4,6-8
            2-3,4-5
            5-7,7-9
            2-8,3-7
            6-6,4-6
            2-6,4-8
          |]

instance Arbitrary Range where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    pure (Range (min a1 a2) (max a1 a2))

generateFourDifferentSorted :: (Ord a, Arbitrary a) => Gen (a,a,a,a)
generateFourDifferentSorted = do
                a1 <- arbitrary
                a2 <- suchThat arbitrary (a1 /=)
                a3 <- suchThat arbitrary (\a -> a1 /=a && a2 /= a)
                a4 <- suchThat arbitrary (\a -> a1 /=a && a2 /= a && a3 /= a )
                let four = [a1,a2,a3,a4]
                let a = minimum four
                let b = minimum (delete a four)
                let c = minimum (four \\ [a, b])
                let d = minimum (four \\ [a,b,c])
                pure (a,b,c,d)

generateNonOverlapping :: Gen (Range, Range)
generateNonOverlapping = fmap (\ (a, b, c, d) -> (Range a b, Range c d)) generateFourDifferentSorted

generatePartiallyOverlapping :: Gen (Range, Range)
generatePartiallyOverlapping = oneof [
     fmap (\ (a, b, c, d) -> (Range a c, Range b d)) generateFourDifferentSorted,
     fmap (\ (a, b, _, d) -> (Range a b, Range b d)) generateFourDifferentSorted
     ]

spec :: Spec
spec = describe "Day4" $ do
  it "count overlapping" $
    pureProgram input `shouldBe` (Report 2 4)

  it "overlap single selection" $
    checkOverlap (Range 5 7) (Range 7 9) `shouldBe` Partial

  it "do not ovelap" $
    checkOverlap (Range 2 4) (Range 6 8) `shouldBe` NoOverlap

  it "do not ovelap" $
    checkOverlap (Range 2 3) (Range 4 5) `shouldBe` NoOverlap

  it "full overlap (edge)" $
    checkOverlap (Range 2 3) (Range 2 5) `shouldBe` Full

  it "full overlap (internal)" $
    checkOverlap (Range 2 5) (Range 3 4) `shouldBe` Full

  it "parse successful lines" $
    parse input `shouldBe` [
         Round (Range 2 4) (Range 6 8),
         Round (Range 2 3) (Range 4 5),
         Round (Range 5 7) (Range 7 9),
         Round (Range 2 8) (Range 3 7),
         Round (Range 6 6) (Range 4 6),
         Round (Range 2 6) (Range 4 8)
    ]

  it "fail to parse lines with no ranges" $
     parse "2-3 4-5" `shouldBe` []

  it "parse lines with comma and unexpected spaces" $
     parse "2-3, 4-5" `shouldBe` [Round (Range 2 3 ) (Range 4 5)]

  it "fail to parse lines with invalid range" $
     parse "23,4-5" `shouldBe` []

  it "fail to parse lines non-numeric ranges" $
     parse "2-x,4-5" `shouldBe` []


  prop "every range overlaps with itself fully" $
    \r -> checkOverlap r r == Full

  prop "for every pair of ranhges, checkOverlap commutes" $
    \r1 r2 -> checkOverlap r1 r2 == checkOverlap r2 r1

  prop "given for different points a < b < c < d, [a,b] and [c,d] are always disjoin" $
    forAll generateNonOverlapping $ \ (r1,r2) ->
              checkOverlap r1 r2  == NoOverlap

  prop "given for different points a < b < c < d, [a,c] and [b,d] are partially overlapping" $
    forAll generatePartiallyOverlapping $ \ (r1,r2) ->
              checkOverlap r1 r2  == Partial

