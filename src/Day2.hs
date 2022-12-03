{-# LANGUAGE DerivingVia #-}
module Day2 where

program :: String -> IO ()
program _ = pure ()

data Move = Rock | Paper | Scissors deriving (Eq,Show, Bounded, Enum)

newtype Score = Score Int deriving (Eq, Show, Num, Ord) via Int 

data Match = Match Move Move deriving (Eq,Show)

data Winner = Player1 | Player2 | Draw deriving (Eq,Show)

data Scores = Scores {
   score1 :: Score, 
   score2 :: Score 
   } deriving (Eq,Show)

score :: Move -> Score 
score = (+ 1) . fromIntegral . fromEnum
--score Rock = 1
--score Paper = 2
--score Scissors = 3

--winnerScore :: Winner -> Score
--scoreOutcome = (* 3) . fromEnum
--scoreOutcome Loss = 0 
--scoreOutcome Draw = 3
--scoreOutcome Win = 6

parsePlayer1 :: Char -> Maybe Move
parsePlayer1 'A' = Just Rock 
parsePlayer1 'B' = Just Paper
parsePlayer1 'C' = Just Scissors 
parsePlayer1 _ = Nothing 

parsePlayer2 :: Char -> Maybe Move
parsePlayer2 'X' = Just Rock 
parsePlayer2 'Y' = Just Paper
parsePlayer2 'Z' = Just Scissors 
parsePlayer2 _ = Nothing 

winner :: Match -> Winner 
winner (Match m1 m2) | m2 == m1 = Draw
                     | m2 == succ m1 = Player2
                     | otherwise = Player1

