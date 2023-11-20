{-# LANGUAGE QuasiQuotes #-}

module Day9Spec where

import Day9
import NeatInterpolation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

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
    $ (fmap ropeTail . evolveRope initialRope . explodeAll)
      [ Instruction RightWard 4
      , Instruction Up 4
      , Instruction LeftWard 3
      , Instruction Down 1
      , Instruction RightWard 4
      , Instruction Down 1
      , Instruction LeftWard 5
      , Instruction RightWard 2
      ]
    `shouldBe` [ Position 0 0
               , Position 0 0
               , Position 1 0
               , Position 2 0
               , Position 3 0
               , Position 3 0
               , Position 4 1
               , Position 4 2
               , Position 4 3
               , Position 4 3
               , Position 3 4
               , Position 2 4
               , Position 2 4
               , Position 2 4
               , Position 2 4
               , Position 3 3
               , Position 4 3
               , Position 4 3
               , Position 4 3
               , Position 4 3
               , Position 3 2
               , Position 2 2
               , Position 1 2
               , Position 1 2
               , Position 1 2
               ]
