{-# LANGUAGE DerivingVia #-}
module Day1 where

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

pureProgram :: T.Text -> Maybe Report
pureProgram = fmap logic  . parse

newtype ElfName = ElfName Int
   deriving (Eq, Num,Show) via Int

newtype Calories = Calories Int
   deriving (Eq, Num, Ord, Read, Show) via Int

data Elf = Elf {
   name :: ElfName,
   calories :: [Calories]
   } deriving (Eq,Show)


totalCalories :: Elf -> Calories
totalCalories = sum . calories

data Report = Report {
    bestCandidate :: Elf,
    allCandidates :: [Elf]
    } deriving (Eq,Show)

parse :: T.Text -> Maybe (N.NonEmpty Elf)
parse =  N.nonEmpty . fmap parseElf . indexed . splitWhen T.null . T.lines  where
   parseElf :: (Int, [T.Text]) -> Elf  -- ignore errors
   parseElf = Elf <$> (ElfName . (+ 1) . fst) <*> (fmap tread . snd)
   tread =  either (const 0) (Calories . fst) . decimal

logic :: N.NonEmpty Elf -> Report
logic = uncurry Report . ((,) <$> N.head <*> N.take 3)  . N.reverse . N.sortWith totalCalories

allCalories :: [Elf] -> Calories
allCalories = sum . concatMap calories

printResultsFromReport :: Report -> String
printResultsFromReport r = printResults
    ((show . name . bestCandidate) r) ((show . totalCalories . bestCandidate) r) ((show . allCalories . allCandidates ) r ) where
     printResults n t a = "Winner: " ++ n ++ "\nTotal calories: " ++ t ++ "\nTotal candidates first 3 candidates: " ++ a

printResultsOrError :: Maybe Report -> T.Text
printResultsOrError = maybe "Parsing Error" (T.pack . printResultsFromReport)
