{-# LANGUAGE QuasiQuotes #-}
module Day1Spec where

import Day1
import Data.Text (unpack)
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
     it "calculate report" $

        pureProgram [trimming|
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
          |] `shouldBe` Just ( Report [(Elf 4 [7000, 8000, 9000]), (Elf 3 [5000, 6000]),(Elf 5 [10000])])

     it "render report" $

        (printResultsOrError . pureProgram) [trimming|
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
          |] `shouldBe` [trimming|
            Winner: 4
            Total calories: 24000
            Total candidates first 3 candidates: 45000
          |]

