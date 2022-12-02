{-# LANGUAGE DerivingVia #-}
module Day1 where

import qualified Data.Text as T
import Data.Text.IO as T
import Control.Monad
import Data.List.NonEmpty
import Data.Sequence.NonEmpty (sortOn)
import Prelude hiding (head)

program :: FilePath -> IO ()
program = longStory



-- unsafe, faster, shorter version
shortStory :: FilePath -> IO ()
shortStory = undefined

-- safer, longer version
longStory :: FilePath -> IO ()
longStory = (=<<) render . fmap logic . elfs

newtype ElfName = ElfName Int deriving (Eq,Show)
   deriving (Num) via Int

newtype Calories = Calories Int deriving (Eq,Show, Ord)
   deriving (Num) via Int

data Elf = Elf {
   name :: ElfName,
   calories :: [Calories]
   } deriving (Eq,Show)

newtype Rep a = Rep [[a]] deriving (Eq,Show)


split :: Eq a => a -> [Maybe a] -> [[a]]
split a = foldr [[]]


-- Maybe a -> [[a]]
-- Maybe  <> [1,2

elfs :: FilePath -> IO (NonEmpty Elf)
elfs = undefined

totalCalories :: Elf -> Calories
totalCalories = sum . calories

logic :: NonEmpty Elf -> Elf
logic = head . sortWith totalCalories

render :: Elf -> IO ()
render e = (T.putStrLn . T.pack) $ show (name e) ++ "\n" ++ show (totalCalories e)












