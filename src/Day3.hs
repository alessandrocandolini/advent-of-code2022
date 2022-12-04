{-# LANGUAGE DerivingVia #-}

module Day3 where

import Data.Char
import qualified Data.List.Split as SP
import qualified Data.Set as S
import Data.String
import qualified Data.Text as T
import qualified Data.Text.IO as T

program :: FilePath -> IO ()
program = (=<<) print . fmap pureProgram . T.readFile

pureProgram :: T.Text -> Result
pureProgram = uncurry Result . generateReport . parse
  where
    generateReport = (,) <$> foldMap rucksackPriority <*> prioritiesGroupOfElves

newtype Item = Item Char deriving (Eq, Show, Ord) via Char

newtype Priority = Priority Int deriving (Eq, Show, Num) via Int

instance Semigroup Priority where
  (<>) = (+)

instance Monoid Priority where
  mempty = 0

data Rucksack = Rucksack
  { compartment1 :: [Item],
    compartment2 :: [Item]
  }
  deriving (Eq, Show)

parse :: T.Text -> [Rucksack]
parse = fmap (uncurry Rucksack . splitHalf . fmap Item . T.unpack) . T.lines
  where
    splitHalf :: [a] -> ([a], [a])
    splitHalf l = splitAt ((length l + 1) `div` 2) l

newtype Overlap = Overlap {items :: S.Set Item} deriving (Eq, Show)

overlap :: [Item] -> Overlap
overlap = Overlap . S.fromList

instance IsString Overlap where
  fromString = Overlap . S.fromList . fmap Item

compartmentOverlap :: Rucksack -> Overlap
compartmentOverlap = overlap . (intersect <$> compartment1 <*> compartment2)

intersect :: (Ord a) => [a] -> [a] -> [a]
intersect [] = const []
intersect xs = filter (`elem` xs)

priority :: Item -> Priority
priority (Item c) = Priority (charToPriority c)
  where
    charToPriority c'
      | isLower c' = fromEnum c' - offsetLowercase
      | otherwise = fromEnum c' - offsetUppercase
    offsetLowercase = fromEnum 'a' - 1
    offsetUppercase = fromEnum 'A' - 27

priorities :: Overlap -> Priority
priorities = foldMap priority . items

rucksackPriority :: Rucksack -> Priority
rucksackPriority = priorities . compartmentOverlap

-- part 2

data Result = Result
  { part1 :: Priority,
    part2 :: Priority
  }
  deriving (Eq, Show)

intersectN :: Ord a => [[a]] -> [a]
intersectN = foldr1 intersect

newtype ElvesGroup = ElvesGroup
  { rucksacks :: [Rucksack]
  }
  deriving (Eq, Show)

allItems :: Rucksack -> [Item]
allItems = (++) <$> compartment1 <*> compartment2

elfGroupOverlap :: ElvesGroup -> Overlap
elfGroupOverlap = overlap . intersectN . fmap allItems . rucksacks

groupByElves :: [Rucksack] -> [ElvesGroup]
groupByElves = fmap ElvesGroup . SP.chunksOf 3

prioritiesGroupOfElves :: [Rucksack] -> Priority
prioritiesGroupOfElves = foldMap (priorities . elfGroupOverlap) . groupByElves
