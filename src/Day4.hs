module Day4 where
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.List.Split (splitOn)
import Data.Maybe
import Control.Monad

program :: FilePath -> IO ()
program = (=<<) printReport . fmap pureProgram . T.readFile

data Report = Report {
  part1 :: [Overlap], part2 :: [Overlap] }  deriving (Eq, Show)

solution1 = length . part1
solution2 = length . part2

printReport r = putStrLn "Solution 1:" >>= \_ -> print (solution1 r) >>= \_ ->  putStrLn " " >>= \_ ->  print (solution2 r)

pureProgram :: T.Text -> Report
pureProgram = uncurry Report . ((,) <$> p1 <*> p2) . fmap overlap . parse where
    p1 = mfilter (Fully ==)
    p2 = mfilter (No /=)

parse :: T.Text -> [Round]
parse = mapMaybe (parseLine .T.unpack) . T.lines

parseLine :: String -> Maybe Round
parseLine = toRound . mapMaybe parseRange . splitOn "," where
  toRound :: [Range] -> Maybe Round
  toRound [h, h2] = Just (Round h h2)
  toRound _ = Nothing

data Round = Round {
  range1 :: Range,
  range2 :: Range } deriving (Eq,Show)

data Range = Range {
  start :: Int,
  end :: Int
  } deriving (Eq,Show)

parseRange :: String -> Maybe Range
parseRange = toRange . fmap read . splitOn "-" where
  toRange (s:e:[]) = Just (Range s e)
  toRange _ = Nothing


data Overlap = Fully | Partially | No deriving (Eq, Show)

checkOverlap :: Range -> Range -> Overlap
checkOverlap (Range a b) (Range c d) | a <= c && d <= b = Fully
                                         | c <= a && b <= d = Fully
                                         | a <= c && c <= b = Partially
                                         | c <= a && a <= d = Partially
                                         | otherwise = No

overlap :: Round -> Overlap
overlap = checkOverlap <$> range1 <*> range2
