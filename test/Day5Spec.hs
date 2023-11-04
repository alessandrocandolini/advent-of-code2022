{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Day5Spec where

import qualified Data.IntMap as M
import Data.List (transpose, unfoldr)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, isNothing)
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day5
import GHC.Exts (IsList (fromList, toList), Item)
import NeatInterpolation
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property
import Text.Megaparsec (parse)

input =
  [trimming|
            [D]
        [N] [C]
        [Z] [M] [P]
         1   2   3

        move 1 from 2 to 1
        move 3 from 1 to 3
        move 2 from 2 to 1
        move 1 from 1 to 2
          |]

inputPart1 =
  [text|
    [D]
[N] [C]
[Z] [M] [P]
 1   2   3
|]

stackToList :: Stack a -> [a]
stackToList = unfoldr pop

instance IsList (Stack a) where
  type Item (Stack a) = a
  fromList = stackFromList
  toList = stackToList

instance IsList (Cargo a) where
  type Item (Cargo a) = (Int, Stack a)
  fromList = Cargo . fromList
  toList = toList . stacks

instance Arbitrary Move where
  arbitrary = Move <$> arbitrary <*> arbitrary

instance (Arbitrary a) => Arbitrary (Stack a) where
  arbitrary = stackFromList <$> arbitrary

instance Arbitrary Crate where
  arbitrary = Crate <$> arbitrary

instance (Arbitrary a) => Arbitrary (Cargo a) where
  arbitrary = cargoFromList <$> listOf ((,) <$> choose (-20, 40) <*> arbitrary)

-- we need a canonical representation for empty crane: either key present and stack empty or key not present at all
cargoFromList :: [(Int, Stack a)] -> Cargo a
cargoFromList = Cargo . M.filter isEmpty . M.fromList
 where
  isEmpty Empty = True
  isEmpty (Day5.NonEmpty _ _) = False

reverseMove :: Move -> Move
reverseMove (Move k1 k2) = Move k2 k1

cargoSize :: Cargo a -> Int
cargoSize = getSum . foldMap (Sum . stackSize) . M.elems . stacks

spec :: Spec
spec = describe "Day 5" $ do
  it "stack from list"
    $ ["A", "B", "C"]
    `shouldBe` push "A" (push "B" (push "C" Empty))

  it "stack to list"
    $ toList (push "A" (push "B" (push "C" Empty)))
    `shouldBe` ["A", "B", "C"]

  it "size of empty stack"
    $ stackSize []
    `shouldBe` 0

  it "size of nonempty stack"
    $ stackSize ["A", "B", "C"]
    `shouldBe` 3

  it "cargo from list"
    $ [(1, ["A", "B", "C"]), (2, ["D", "E"]), (3, [])]
    `shouldBe` Cargo (M.fromList [(1, push "A" (push "B" (push "C" Empty))), (2, push "D" (push "E" Empty)), (3, Empty)])

  it "perform a valid re-arrange"
    $ let
        cargo = [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]
        expected = [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
       in
        rearrange (Move 2 1) cargo `shouldBe` expected

  it "perform multiple valid re-arranges"
    $ let
        cargo = [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]
        moves = [Move 2 1, Move 1 3, Move 1 3, Move 1 3, Move 2 1, Move 2 1, Move 1 2]
        expected = [(1, ["C"]), (2, ["M"]), (3, ["Z", "N", "D", "P"])]
       in
        rearrangeAll moves cargo `shouldBe` expected

  it "perform an invalid re-arrange"
    $ let
        cargo = [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, [])]
       in
        rearrange (Move 3 1) cargo `shouldBe` cargo

  it "peaks returns the top elements of each stack"
    $ let
        cargo = [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]
       in
        peeks cargo `shouldBe` ["N", "D", "P"]

  it "peaks ignores empty stacks when returning the top elements of each stack"
    $ let
        cargo = [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, []), (4, ["P"])]
       in
        peeks cargo `shouldBe` ["N", "D", "P"]

  it "example of composing move and its reverse"
    $ let
        m = Move 0 (-1)
        c = Cargo $ fromList [(0, ['a', 'b'])]
       in
        (rearrange (reverseMove m) . rearrange m) c `shouldBe` c

  prop "rearrange is the identity when source and destination coincide"
    $ \k c -> rearrange (Move k k) c == (c :: Cargo Char)

  prop "reversing the move brings the cargo to the original state"
    $ \m c ->
      (rearrange (reverseMove m) . rearrange m) c
        == c
        || (rearrange (reverseMove m) . rearrange m) c
        == rearrange (reverseMove m) (c :: Cargo Char)

  prop "size is left unchanged after an operation"
    $ \m c -> cargoSize (rearrange m c) == cargoSize (c :: Cargo Char)

  it "parseInstruction parse correctly a valid text"
    $ parseInstruction "move 1 from 7 to 4"
    `shouldBe` Just (Instruction 1 7 4)

  it "parseInstructions parse correctly a sequence of valid text"
    $ parseInstructions
      [trimming|
          move 1 from 2 to 1
          move 3 from 1 to 3
          move 2 from 2 to 1
          move 1 from 1 to 2
         |]
    `shouldBe` [Instruction 1 2 1, Instruction 3 1 3, Instruction 2 2 1, Instruction 1 1 2]

  prop "parseInstruction parse correctly any valid text"
    $ \a b c ->
      let
        a' = getPositive a
        b' = getPositive b
        c' = getPositive c
        s = "move " ++ show a' ++ " from " ++ show b' ++ " to " ++ show c'
       in
        parseInstruction s `shouldBe` Just (Instruction a' b' c')

  it "parse crate"
    $ let
        s = "[C]"
       in
        parse crateP "" s `shouldBe` Right (Crate 'C')


  it "parse optional crates when they are all defined (and no spaces in between)"
    $ let
        s = "[C][D][E][F]"
       in
        parse cratesP "" s `shouldBe` Right [Just (Crate 'C'), Just (Crate 'D'), Just (Crate 'E'), Just (Crate 'F')]

  it "parse optional crates when they are all defined (and spaces in between)"
    $ let
        s = "[C] [D] [E] [F]"
       in
        parse cratesP "" s `shouldBe` Right [Just (Crate 'C'), Just (Crate 'D'), Just (Crate 'E'), Just (Crate 'F')]

  it "parse optional crates when they are all defined (and spaces in between, at the beginning and at the end)"
    $ let
        s = " [C] [D] [E] [F] "
       in
        parse cratesP "" s `shouldBe` Right [Just (Crate 'C'), Just (Crate 'D'), Just (Crate 'E'), Just (Crate 'F')]

  it "parse optional crates when they are not all defined"
    $ let
        s = "[C] [D]     [E]     [F] "
       in
        parse cratesP "" s `shouldBe` Right [Just (Crate 'C'), Just (Crate 'D'), Nothing, Just (Crate 'E'), Nothing, Just (Crate 'F')]

  it "parse optional crates when they are not all defined, starting and ending with undefined"
    $ let
        s = "    [C] [D] [E]     [F]    "
       in
        parse cratesP "" s `shouldBe` Right [Nothing, Just (Crate 'C'), Just (Crate 'D'), Just (Crate 'E'), Nothing, Just (Crate 'F'), Nothing]

  it "parse optional crates when they are not all defined, starting and ending with undefined, in presence of additional space at the beginning and at the end"
    $ let
        s = "     [C] [D] [E]     [F]     "
       in
        parse cratesP "" s `shouldBe` Right [Nothing, Just (Crate 'C'), Just (Crate 'D'), Just (Crate 'E'), Nothing, Just (Crate 'F'), Nothing]

  it "parse optional crates from multiple lines"
    $ let
        s = T.pack $ unlines ["    [D]    ","[N] [C]    ","[Z] [M] [P]"]
        expected :: [[Maybe Crate]]
        expected =
          [ [Nothing, Just (Crate 'D'), Nothing]
          , [Just (Crate 'N'), Just (Crate 'C'), Nothing]
          , [Just (Crate 'Z'), Just (Crate 'M'), Just (Crate 'P')]
          ]
       in
        parse allCratesP "" s `shouldBe` Right expected

  it "parse crates as maps from multiple lines"
    $ let
        s = T.pack $ unlines ["    [D]    ","[N] [C]    ","[Z] [M] [P]"]
        expected :: Cargo Crate
        expected = Cargo $ M.fromList
          [ (1, [Crate 'N', Crate 'Z'])
          , (2, [Crate 'D', Crate 'C', Crate 'M'])
          , (3, [Crate 'P'])
          ]
       in
        parseCrates s `shouldBe` Right expected

  it "parse example input"
    $ let
        cargo = [(1, [Crate 'N', Crate 'Z']), (2, [Crate 'D', Crate 'C', Crate 'M']), (3, [Crate 'P'])]
        instructions = [Instruction 1 2 1, Instruction 3 1 3, Instruction 2 2 1, Instruction 1 1 2]
        input = T.pack $ unlines ["     [D] ", " [N] [C] ", " [Z] [M] [P] ", " 1   2   3 ","\n", " move 1 from 2 to 1", " move 3 from 1 to 3"," move 2 from 2 to 1"," move 1 from 1 to 2"]
       in
        parseInput input `shouldBe` Just (cargo, instructions)

  it "run"
    $ let
        input = T.pack $ unlines ["     [D] ", " [N] [C] ", " [Z] [M] [P] ", " 1   2   3 ","\n", " move 1 from 2 to 1", " move 3 from 1 to 3"," move 2 from 2 to 1"," move 1 from 1 to 2"]
       in
        run input `shouldBe` Just (Answer "CMZ")

  it "run on a extra small example" $ do
    input <- T.readFile "resources/input5-extra-small"
    run input `shouldBe` Just (Answer "TSDQMFCMM")

  it "run on a small example" $ do
    input <- T.readFile "resources/input5-small"
    run input `shouldBe` Just (Answer "THDQMTCNM")

