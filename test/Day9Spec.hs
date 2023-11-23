{-# LANGUAGE QuasiQuotes #-}

module Day9Spec where

import Day9
import NeatInterpolation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import qualified Data.List.NonEmpty as N

spec :: Spec
spec = describe "Day 9" $ do
  it "parse instruction"
    $ parseInstruction
      [trimming|
          L 1
          U 3
          R 1
          D 2
          |]
    `shouldBe` [ Instruction LeftWard 1
               , Instruction Up 3
               , Instruction RightWard 1
               , Instruction Down 2
               ]

  it "evolveRope"
    $ (fmap (N.last . knots) . evolveRope part1 . explodeAll)
      [ Instruction RightWard 4
      , Instruction Up 4
      , Instruction LeftWard 3
      , Instruction Down 1
      , Instruction RightWard 4
      , Instruction Down 1
      , Instruction LeftWard 5
      , Instruction RightWard 2
      ]
    `shouldBe` [ Knot 0 0
               , Knot 0 0
               , Knot 1 0
               , Knot 2 0
               , Knot 3 0
               , Knot 3 0
               , Knot 4 1
               , Knot 4 2
               , Knot 4 3
               , Knot 4 3
               , Knot 3 4
               , Knot 2 4
               , Knot 2 4
               , Knot 2 4
               , Knot 2 4
               , Knot 3 3
               , Knot 4 3
               , Knot 4 3
               , Knot 4 3
               , Knot 4 3
               , Knot 3 2
               , Knot 2 2
               , Knot 1 2
               , Knot 1 2
               , Knot 1 2
               ]
