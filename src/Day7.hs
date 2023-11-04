{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingVia #-}

module Day7 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Exts (IsString)
import Data.Semigroup
import Data.Foldable

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer deriving (Eq, Show)

newtype DirectoryName = DirectoryName String
  deriving (Eq, Show)
  deriving (IsString) via String

newtype FileName = FileName String
  deriving (Eq, Show)
  deriving (IsString) via String

newtype Size = Size Int
  deriving (Eq,Show)
  deriving (Ord, Num, Semigroup, Monoid) via (Sum Int)

data FileSystem a
  = File FileName a
  | Directory DirectoryName [FileSystem a]
  deriving (Eq, Show, Foldable)

logic :: T.Text -> Answer
logic = const Answer

du :: Monoid m => FileSystem m -> m
du = fold

