module Day2Spec where

import Day2
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck.Arbitrary


instance Arbitrary Move where
   arbitrary = arbitraryBoundedEnum


spec :: Spec
spec = describe "Day2: Play rock-paper-scissors" $ do

     it "paper wins over rock" $
        winner (Match Rock Paper) `shouldBe` Player2

     it "rock wins over scissors" $
        winner (Match Rock Scissors) `shouldBe` Player1

     it "rock rock is a draw" $
        winner (Match Rock Rock) `shouldBe` Draw

     it "scissors win over paper " $
        winner (Match Scissors Paper) `shouldBe` Player1

     prop "always result in draw if the moves are the same for all players" $
        \m -> winner (Match m m) == Draw

     prop "never result in draw if the moves are not the same for the two players" $
        \m1 m2  -> m1 /= m2 ==> winner (Match m1 m2) /= Draw
     
     prop "winner is commutative" $
        \m1 m2  -> m1 /= m2 ==> (
          (winner (Match m1 m2) == Player1  && 
          winner (Match m2 m1) == Player2)  || 
           (winner (Match m1 m2) == Player2  && 
            winner (Match m2 m1) == Player1))


     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

