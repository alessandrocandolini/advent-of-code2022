module Day6 where

import Data.List (find, nub, tails)
import qualified Data.Text as T
import qualified Data.Text.IO as T

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

logic :: T.Text -> (Maybe Int, Maybe Int)
logic = ((,) <$> findFirst 4 <*> findFirst 14) . T.unpack

findFirst :: Eq a => Int -> [a] -> Maybe Int
findFirst n = fmap ((+ n) . fst) . find (allDistinct . snd) . zip [0 ..] . fmap (take n) . tails
 where
  allDistinct = (==) <$> length <*> length . nub
