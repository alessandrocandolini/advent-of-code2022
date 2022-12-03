{-# LANGUAGE DerivingVia #-}

module Day2 (program, pureProgram, score, play, Player (..), Move (..), Score (..), Match (..), Winner (..)) where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (mapMaybe)

program :: FilePath -> IO ()
program = (=<<) print  . fmap pureProgram . T.readFile

pureProgram :: T.Text -> Score
pureProgram = scores Player2 . parseMatches

data Move = Rock | Paper | Scissors deriving (Eq, Show, Bounded, Enum)

data Match = Match
  { movePlayer1 :: Move,
    movePlayer2 :: Move
  }
  deriving (Eq, Show)

data Player = Player1 | Player2 deriving (Eq, Show, Enum, Bounded)

newtype Score = Score Int deriving (Eq, Show, Num, Ord) via Int

instance Semigroup Score  where
   (<>) = (+)

instance Monoid Score where
   mempty = 0

data Winner = Winner Player | NoWinner deriving (Eq, Show)

data Outcome = Lose | Draw | Win deriving (Show, Eq, Enum, Bounded)

cyclicSucc :: (Eq a, Enum a, Bounded a) => a -> a
cyclicSucc a
  | a == maxBound = minBound
  | otherwise = succ a

scoreMove :: Move -> Score
scoreMove = (+ 1) . fromIntegral . fromEnum

--score Rock = 1
--score Paper = 2
--score Scissors = 3

scoreOutcome :: Outcome -> Score
scoreOutcome = (* 3) . fromIntegral . fromEnum

--scoreOutcome Loss = 0
--scoreOutcome Draw = 3
--scoreOutcome Win = 6

--newtype Parser a = Parser {
  --unparse :: StateT String Maybe a
  --} deriving (Functor, Applicative, Alternative, Monad, MonadPlus)


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

parseMatch :: T.Text -> Maybe Match
parseMatch = parseThree  . T.unpack where
   parseThree (a : ' ' :  b : [])  = Match <$> parse1 a <*> parse2 b
   parseThree _ = Nothing

parseMatches :: T.Text -> [Match]
parseMatches = mapMaybe parseMatch . T.lines

move :: Player -> Match -> Move
move Player1 = movePlayer1
move Player2 = movePlayer2

play :: Match -> Winner
play (Match m1 m2)
  | m2 == m1 = NoWinner
  | m2 == cyclicSucc m1 = Winner Player2
  | otherwise = Winner Player1

outcome :: Player -> Match -> Outcome
outcome p = checkWinner . play
  where
    checkWinner NoWinner = Draw
    checkWinner (Winner p')
      | p' == p = Win
      | otherwise = Lose

score :: Player -> Match -> Score
score p = (+) <$> scoreOutcome . outcome p <*> scoreMove . move p

scores :: Player -> [Match] -> Score
scores p = foldMap (score p)
