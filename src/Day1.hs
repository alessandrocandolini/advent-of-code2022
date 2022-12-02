{-# LANGUAGE DerivingVia #-}
module Day1 where

import qualified Data.Text as T
import Data.Text.IO as T
import Control.Monad
import Data.List.NonEmpty
import Data.Sequence.NonEmpty (sortOn)
import Prelude hiding (reverse, head)
import Control.Exception
import UnliftIO
import Data.Either.Combinators (maybeToRight)
import Data.List.Split
import Data.List.Index

program :: FilePath -> IO ()
program = longStory



-- unsafe, faster, shorter version
oneLiner :: FilePath -> IO ()
oneLiner = fmap (fmap unpack . lines) . T.readFile

-- safer, longer version
longStory :: FilePath -> IO ()
longStory = (=<<) printResults . fmap logic . fetchElves

newtype ElfName = ElfName Int deriving (Eq,Show)
   deriving (Num) via Int

newtype Calories = Calories Int deriving (Eq,Show, Ord)
   deriving (Num, Read) via Int

data ParseFail = ParseFail deriving (Eq,Show)

instance Exception ParseFail 
  
data Elf = Elf {
   name :: ElfName,
   calories :: [Calories]
   } deriving (Eq,Show)

fetchElves :: FilePath -> IO (NonEmpty Elf)
fetchElves = fromEitherIO . fmap (maybeToRight ParseFail . parse) . T.readFile 

parse :: T.Text -> Maybe (NonEmpty Elf) 
parse =  nonEmpty . fmap parseElf . indexed . splitWhen null . fmap T.unpack . T.lines  where 

   parseElf :: (Int, [String]) -> Elf  -- ignore errors 
   parseElf (i,c) = Elf (ElfName (i +1)) (fmap read c )



totalCalories :: Elf -> Calories
totalCalories = sum . calories

logic :: NonEmpty Elf -> Elf
logic = head . reverse . sortWith totalCalories

printResults :: Elf -> IO ()
printResults e = (T.putStrLn . T.pack) $ show (name e) ++ "\n" ++ show (totalCalories e)













