{-# LANGUAGE QuasiQuotes #-}
module Day1Spec where

import Day1
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import NeatInterpolation
import Data.Text (Text)
import qualified Data.List.NonEmpty as N

spec :: Spec
spec = describe "Simple test" $ do

     it "parse elves" $
        parse [trimming| 
          1000
          2000
          3000

          4000

          5000
          6000

          7000
          8000
          9000

          10000
          |] `shouldBe` Just [
               Elf 1 [1000, 2000, 3000],
               Elf 2 [4000],
               Elf 3 [5000, 6000],
               Elf 4 [7000, 8000, 9000],
               Elf 5 [10000]
            ]
     it "calculate result" $
        (fmap logic . parse) [trimming| 
          1000
          2000
          3000

          4000

          5000
          6000

          7000
          8000
          9000

          10000
          |] `shouldBe` Just (Elf 4 [7000, 8000, 9000])

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])

