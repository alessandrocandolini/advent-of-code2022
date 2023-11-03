{-# LANGUAGE QuasiQuotes #-}
module Day6Spec where

import Day6
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation

spec :: Spec
spec = describe "Day 6" $ do

     it "findFirst example 1" $
        findFirst 4 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` (Just 7)

     it "findFirst example 2" $
        findFirst 4 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` (Just 5)

     it "findFirst example 3" $
        findFirst 4 "nppdvjthqldpwncqszvftbrmjlhg" `shouldBe` (Just 6)

     it "findFirst example 4" $
        findFirst 4 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" `shouldBe` (Just 10)

     it "findFirst example 5" $
        findFirst 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" `shouldBe` (Just 11)

     it "findFirst example 6" $
        findFirst 14 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` (Just 19)

     it "findFirst example 7" $
        findFirst 14 "bvwbjplbgvbhsrlpgdmjqwftvncz" `shouldBe` (Just 23)


     prop "" $
        \l -> reverse ( reverse l ) == ( l::[Int])

