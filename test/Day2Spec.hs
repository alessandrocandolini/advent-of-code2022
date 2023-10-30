{-# LANGUAGE QuasiQuotes #-}
module Day2Spec where

import Day2
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property
import NeatInterpolation

instance Arbitrary Move where
  arbitrary = arbitraryBoundedEnum

spec :: Spec
spec = describe "Day2: Play rock-paper-scissors" $ do
  it "paper wins over rock" $
    winner (Round Rock Paper) `shouldBe` Just Player2

  it "rock wins over scissors" $
    winner (Round Rock Scissors) `shouldBe` Just Player1

  it "rock rock is a draw" $
    winner (Round Rock Rock) `shouldBe` Nothing

  it "scissors win over paper " $
    winner (Round Scissors Paper) `shouldBe` Just Player1

  prop "always result in draw if the moves are the same for all players" $
    \m -> winner (Round m m) == Nothing

  prop "never result in draw if the moves are not the same for the two players" $
    \m1 m2 -> m1 /= m2 ==> winner (Round m1 m2) /= Nothing

  prop "winner is symmetric" $
    \m1 m2 ->
      m1 /= m2
        ==> ( ( winner (Round m1 m2) == Just Player1
                  && winner (Round m2 m1) == Just Player2
              )
                || ( winner (Round m1 m2) == Just Player2
                       && winner (Round m2 m1) == Just Player1
                   )
            )

  it "example" $
     logic [trimming|
        A Y
        B X
        C Z
        |] `shouldBe` (Answer 15 12)
