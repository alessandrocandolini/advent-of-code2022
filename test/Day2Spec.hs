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
    play (Match Rock Paper) `shouldBe` Winner Player2

  it "rock wins over scissors" $
    play (Match Rock Scissors) `shouldBe` Winner Player1

  it "rock rock is a draw" $
    play (Match Rock Rock) `shouldBe` NoWinner

  it "scissors win over paper " $
    play (Match Scissors Paper) `shouldBe` Winner Player1

  prop "always result in draw if the moves are the same for all players" $
    \m -> play (Match m m) == NoWinner

  prop "never result in draw if the moves are not the same for the two players" $
    \m1 m2 -> m1 /= m2 ==> play (Match m1 m2) /= NoWinner

  prop "play is symmetric" $
    \m1 m2 ->
      m1 /= m2
        ==> ( ( play (Match m1 m2) == Winner Player1
                  && play (Match m2 m1) == Winner Player2
              )
                || ( play (Match m1 m2) == Winner Player2
                       && play (Match m2 m1) == Winner Player1
                   )
            )

  it "example" $
     pureProgram [trimming|
        A Y
        B X
        C Z
        |] `shouldBe` (Report 15 12)
