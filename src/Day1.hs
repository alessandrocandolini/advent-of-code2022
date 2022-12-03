{-# LANGUAGE DerivingVia #-}
module Day1(program, parse, pureProgram, logic, printResultsOrError, Elf(..), ElfName(..), Calories(..), Report(..)) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.List.NonEmpty as N
import Data.List.Split
import Data.List.Index
import Data.Text.Read (decimal)

program :: FilePath -> IO ()
program = (=<<) T.putStrLn
                   . fmap (printResultsOrError . pureProgram)
                   . T.readFile

pureProgram = fmap logic  . parse

newtype ElfName = ElfName Int
   deriving (Eq, Num,Show) via Int

newtype Calories = Calories Int
   deriving (Eq, Num, Ord, Read, Show) via Int

data Elf = Elf {
   name :: ElfName,
   calories :: [Calories]
   } deriving (Eq,Show)


totalCalories = sum . calories

newtype Report = Report {
    candidates :: N.NonEmpty Elf
    } deriving (Eq,Show)

bestCandidate = N.head . candidates

parse =  N.nonEmpty . fmap parseElf . indexed . splitWhen T.null . T.lines  where
   parseElf = Elf <$> (ElfName . (+ 1) . fst) <*> (fmap tRead . snd)
   tRead =  either (const 0) (Calories . fst) . decimal

logic = Report . ((N.:|) <$> N.head <*> (take 2 . N.tail))  . N.reverse . N.sortWith totalCalories

allCalories = sum . concatMap calories

printResultsFromReport = T.pack . (printResults <$> name . bestCandidate <*> totalCalories . bestCandidate <*> allCalories . candidates ) where
     printResults n t a = "Winner: " ++ show n ++ "\nTotal calories: " ++ show t ++ "\nTotal candidates first 3 candidates: " ++ show a

printResultsOrError = maybe "Parsing Error" printResultsFromReport
