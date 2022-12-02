{-# LANGUAGE DerivingVia #-}
module Day1(program) where

import qualified Data.Text as T
import Data.Text.IO as T
import Control.Monad
import Data.List.NonEmpty
import Data.Sequence.NonEmpty (sortOn)
import Prelude hiding (head)

program :: FilePath -> IO ()
program = longStory



-- unsafe, faster, shorter version
oneLiner :: FilePath -> IO ()
oneLiner = undefined

-- safer, longer version
longStory :: FilePath -> IO ()
longStory = (=<<) printResults . fmap logic . fetchElves

newtype ElfName = ElfName Int deriving (Eq,Show)
   deriving (Num) via Int

newtype Calories = Calories Int deriving (Eq,Show, Ord)
   deriving (Num) via Int

data Elf = Elf {
   name :: ElfName,
   calories :: [Calories]
   } deriving (Eq,Show)

fetchElves :: FilePath -> IO (NonEmpty Elf)
fetchElves = undefined

totalCalories :: Elf -> Calories
totalCalories = sum . calories

logic :: NonEmpty Elf -> Elf
logic = head . sortWith totalCalories

printResults :: Elf -> IO ()
printResults e = (T.putStrLn . T.pack) $ show (name e) ++ "\n" ++ show (totalCalories e)












