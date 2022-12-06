{-# LANGUAGE QuasiQuotes #-}
module Day5Spec where

import Day5
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation
import Test.QuickCheck
import qualified Data.Vector as V

input =
  [trimming|
             D]
        [N] [C]
        [Z] [M] [P]
         1   2   3

        move 1 from 2 to 1
        move 3 from 1 to 3
        move 2 from 2 to 1
        move 1 from 1 to 2
          |]

instance Arbitrary Move where
   arbitrary = move <$> arbitrary <*> arbitrary where
               move a1 a2 = Move (Origin (getPositive a1)) (Destination (getPositive a2))

instance Arbitrary a => Arbitrary (Stack a) where
   arbitrary = stackFromList <$> arbitrary where
            stackFromList :: [a] -> Stack a
            stackFromList = foldr push Empty

instance Arbitrary Crate where
   arbitrary = Crate <$> arbitrary

instance Arbitrary Cargo where
   arbitrary = Cargo . V.fromList <$> arbitrary


spec :: Spec
spec = describe "Day 5" $ do

     it "" $
        1 `shouldBe` 1

     prop "rearrange is the identity when source and destination coincide" $
        \m c -> rearrange m c == c

     prop "reversing the move brings the cargo to the original state" $
        \m c -> rearrange (reverseMove m) (rearrange m c) == c

     prop "size and content of a cargo is left unchanged after an operation" $
        \m c -> let
               c' = rearrange m c
            in
               (cargoSize c', content c' ) == (cargoSize c , content c)

