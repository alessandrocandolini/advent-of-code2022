{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import Day3
import NeatInterpolation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Data.String

rucksack s1 s2 = Rucksack (fmap Item s1) (fmap Item s2)

input =
  [trimming|
          vJrwpWtwJgWrhcsFMMfFFhFp
          jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
          PmmdzqPrVvPwwTWBwg
          wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
          ttgJtRGJQctTZtZT
          CrZsJsPPZsGzwwsLwLmpwMDw
          |]

spec :: Spec
spec = describe "Day3" $ do
  it "priority of lowercase p" $
    priority (Item 'p') `shouldBe` 16

  it "priority of lowercase v" $
    priority (Item 'v') `shouldBe` 22

  it "priority of uppercase L" $
    priority (Item 'L') `shouldBe` 38

  it "priority of uppercase P" $
    priority (Item 'P') `shouldBe` 42

  it "parse rucksacks" $
    (fmap compartmentOverlap . parse) input
      `shouldBe` [ "p", "L", "P" , "v", "t", "s"]

  it "part1" $
    (part1 .pureProgram) input `shouldBe` (Priority 157)

  it "group by elves and calculate overlap" $
     (fmap elfGroupOverlap . groupByElves . parse) input `shouldBe` [ "r", "Z"]

  it "parse rucksacks" $
    (part2 .pureProgram) input `shouldBe` (Priority 70)


  prop "property-based unit test" $
    \l -> reverse (reverse l) == (l :: [Int])

