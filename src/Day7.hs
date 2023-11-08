{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}

module Day7 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Exts (IsString)
import Data.Semigroup
import Data.Foldable
import qualified Data.List.NonEmpty as N
import Data.List.NonEmpty (NonEmpty)
import Data.Fix
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as P

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

data FileSystemF a
  = FileF FileName Size
  | DirectoryF DirectoryName [a]
  deriving (Eq, Show, Foldable, Functor)

type FileSystem = Fix FileSystemF

file :: FileName -> Size -> FileSystem
file name = Fix . FileF name

directory :: DirectoryName -> [FileSystem] -> FileSystem
directory name = Fix . DirectoryF name

logic :: T.Text -> Answer
logic = const Answer

duAlgebra :: FileSystemF Size -> Size
duAlgebra (FileF _ size) = size
duAlgebra (DirectoryF _ sizes) = sum sizes

du :: FileSystem -> Size
du = cata duAlgebra

duDir :: FileSystem -> [(DirectoryName, Size)]
duDir = undefined

pretty :: FileSystemF Doc -> Doc
pretty (FileF (FileName name) (Size size)) = P.text name <> P.space <> P.int size
pretty (DirectoryF _ docs) = P.cat docs

display :: FileSystem -> String
display = P.render . cata pretty


-- pretty print
-- exclude files
