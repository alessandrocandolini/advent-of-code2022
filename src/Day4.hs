module Day4 where

import Control.Monad
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Read (readMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap pureProgram . T.readFile

data Report = Report Int Int deriving (Eq, Show)

pureProgram :: T.Text -> Report
pureProgram = uncurry Report . ((,) <$> p1 <*> p2) . fmap overlap . parse
  where
    p1 = length . mfilter (Full ==)
    p2 = length . mfilter (NoOverlap /=)

parse :: T.Text -> [Round]
parse = mapMaybe (parseLine . T.unpack) . T.lines

parseLine :: String -> Maybe Round
parseLine = toRound . mapMaybe parseRange . splitOn ","
  where
    toRound :: [Range] -> Maybe Round
    toRound [h, h2] = Just (Round h h2)
    toRound _ = Nothing

data Round = Round
  { range1 :: Range,
    range2 :: Range
  }
  deriving (Eq, Show)

data Range = Range Int Int deriving (Eq, Show)

parseRange :: String -> Maybe Range
parseRange = toRange . mapMaybe readMaybe . splitOn "-"
  where
    toRange [s, e] = Just (Range s e)
    toRange _ = Nothing

data Overlap = Full | Partial | NoOverlap deriving (Eq, Show)

checkOverlap :: Range -> Range -> Overlap
checkOverlap (Range a b) (Range c d)
  | a <= c && d <= b = Full
  | c <= a && b <= d = Full
  | a <= c && c <= b = Partial
  | c <= a && a <= d = Partial
  | otherwise = NoOverlap

overlap :: Round -> Overlap
overlap = checkOverlap <$> range1 <*> range2
