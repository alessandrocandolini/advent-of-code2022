{-# LANGUAGE QuasiQuotes #-}

module Day9Spec where

import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day9
import NeatInterpolation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

moveTwoKnots :: (Knot, Knot) -> Direction -> (Knot, Knot)
moveTwoKnots (knotH, knotT) direction = (knotH', knotT')
 where
  knotH' = move direction knotH
  knotT' = moveTowards knotH' knotT

instance Arbitrary Direction where
  arbitrary = elements [Up, Down, LeftWard, RightWard]

instance Arbitrary Knot where
  arbitrary = Knot <$> choose (0, 100) <*> choose (0, 100)

instructions =
  [ Instruction RightWard 4
  , Instruction Up 4
  , Instruction LeftWard 3
  , Instruction Down 1
  , Instruction RightWard 4
  , Instruction Down 1
  , Instruction LeftWard 5
  , Instruction RightWard 2
  ]

spec :: Spec
spec = describe "Day 9" $ do
  it "parse instructions"
    $ parseInstructions
      [trimming|
          L 1
          U 3
          R 1
          D 2
          |]
    `shouldBe` Right
      [ Instruction LeftWard 1
      , Instruction Up 3
      , Instruction RightWard 1
      , Instruction Down 2
      ]
  it "parse instructions with trailing newline"
    $ parseInstructions (T.pack (unlines ["L 1", "U 3", "R 1", "D 2"]))
    `shouldBe` Right
      [ Instruction LeftWard 1
      , Instruction Up 3
      , Instruction RightWard 1
      , Instruction Down 2
      ]

  it "parse instructions without trailing newline"
    $ parseInstructions (T.pack "L 1\nU 3\nR 1\nD 2")
    `shouldBe` Right
      [ Instruction LeftWard 1
      , Instruction Up 3
      , Instruction RightWard 1
      , Instruction Down 2
      ]

  prop "model check: evolveRope for two knots is the same as evolving the two knows manually"
    $ \directions ->
      let
        rope = Rope (knot N.:| [knot])
        ropeHistory = evolveRope rope directions
        twoKnots = (knot, knot)
        twoKnotsHistory = scanl moveTwoKnots twoKnots directions
       in
        fmap (N.last . knots) ropeHistory == fmap snd twoKnotsHistory

  prop "moveTowards behaves as the identity function for knots nearby"
    $ \knot direction ->
      let
        knot' = move direction knot
       in
        moveTowards knot knot' == knot'

  it "evolveRope with just two knows (part1)"
    $ (fmap (N.last . knots) . evolveRope rope1 . explodeInstructions) instructions
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

  it "solve example"
    $ (solve rope1 . explodeInstructions) instructions
    `shouldBe` 13

  it "solve the puzzle" $ do
    input <- T.readFile "resources/input9"
    logic input `shouldBe` Answer 6376 2607
