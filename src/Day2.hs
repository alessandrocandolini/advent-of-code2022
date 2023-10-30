{-# LANGUAGE DerivingVia #-}

module Day2 (program, logic, score, winner, Player (..), Move (..), Score (..), Round (..), Answer (..)) where

import Data.Maybe (mapMaybe)
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T

data Answer = Answer
  { part1 :: Score
  , part2 :: Score
  }
  deriving (Eq, Show)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

logic :: T.Text -> Answer
logic =
  Answer
    <$> totalScore Player2
    . parseRounds
    <*> totalScore Player2
    . fmap (uncurry roundFromOutcome)
    . parseMoveAndDesiredOutcomes

data Move = Rock | Paper | Scissors deriving (Eq, Show, Bounded, Enum)

data Player = Player1 | Player2 deriving (Eq, Show)

data Round = Round
  { movePlayer1 :: Move
  , movePlayer2 :: Move
  }
  deriving (Eq, Show)

data Outcome = Lose | Draw | Win deriving (Show, Eq, Enum, Bounded)

newtype Score = Score Int
  deriving (Eq, Show, Num, Ord) via Int
  deriving (Semigroup, Monoid) via (Sum Int)

next :: (Eq a, Enum a, Bounded a) => a -> a
next a
  | a == maxBound = minBound
  | otherwise = succ a

previous :: (Eq a, Enum a, Bounded a) => a -> a
previous a
  | a == minBound = maxBound
  | otherwise = pred a

numFromEnum :: (Enum a, Num b) => a -> b
numFromEnum = fromIntegral . fromEnum

scoreForMove :: Move -> Score
scoreForMove = (+ 1) . numFromEnum

-- score Rock = 1
-- score Paper = 2
-- score Scissors = 3

scoreForOutcome :: Outcome -> Score
scoreForOutcome = (* 3) . numFromEnum

-- scoreOutcome Loss = 0
-- scoreOutcome Draw = 3
-- scoreOutcome Win = 6

parse1 :: Char -> Maybe Move
parse1 'A' = Just Rock
parse1 'B' = Just Paper
parse1 'C' = Just Scissors
parse1 _ = Nothing

parse2 :: Char -> Maybe Move
parse2 'X' = Just Rock
parse2 'Y' = Just Paper
parse2 'Z' = Just Scissors
parse2 _ = Nothing

parseRound :: T.Text -> Maybe Round
parseRound = parseLine . T.unpack
 where
  parseLine [a, ' ', b] = Round <$> parse1 a <*> parse2 b
  parseLine _ = Nothing

parseRounds :: T.Text -> [Round]
parseRounds = mapMaybe parseRound . T.lines

move :: Player -> Round -> Move
move Player1 = movePlayer1
move Player2 = movePlayer2

winner :: Round -> Maybe Player
winner (Round m1 m2)
  | m1 == m2 = Nothing
  | m2 == next m1 = Just Player2
  | otherwise = Just Player1

outcome :: Player -> Round -> Outcome
outcome p = checkWinner . winner
 where
  checkWinner Nothing = Draw
  checkWinner (Just p')
    | p' == p = Win
    | otherwise = Lose

score :: Player -> Round -> Score
score p = (<>) <$> scoreForOutcome . outcome p <*> scoreForMove . move p

totalScore :: Player -> [Round] -> Score
totalScore p = foldMap (score p)

-- part 2

parseDesiredOutcome :: Char -> Maybe Outcome
parseDesiredOutcome 'X' = Just Lose
parseDesiredOutcome 'Y' = Just Draw
parseDesiredOutcome 'Z' = Just Win
parseDesiredOutcome _ = Nothing

parseMoveAndDesiredOutcome :: T.Text -> Maybe (Move, Outcome)
parseMoveAndDesiredOutcome = parseLine . T.unpack
 where
  parseLine [a, ' ', b] = (,) <$> parse1 a <*> parseDesiredOutcome b
  parseLine _ = Nothing

parseMoveAndDesiredOutcomes :: T.Text -> [(Move, Outcome)]
parseMoveAndDesiredOutcomes = mapMaybe parseMoveAndDesiredOutcome . T.lines

moveFromOutcome :: Outcome -> Move -> Move
moveFromOutcome Lose = previous
moveFromOutcome Draw = id
moveFromOutcome Win = next

roundFromOutcome :: Move -> Outcome -> Round
roundFromOutcome m o = Round m (moveFromOutcome o m)
