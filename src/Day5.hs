{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DerivingVia #-}

module Day5 where

import qualified Data.IntMap as M
import Data.Maybe (fromMaybe)
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Safe (readMay)
import Witherable (mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Functor (($>))
import Data.Either.Combinators (rightToMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap (fmap (toAnswer . uncurry (flip logic)) . parseInput) . T.readFile where
  toAnswer :: [Crate] -> Answer
  toAnswer = Answer . fmap name

newtype Answer = Answer String deriving (Eq, Show)

logic :: [Instruction] -> Cargo a -> [a]
logic inst = peeks . rearrangeAll (explodeAll inst)

data Stack a = Empty | NonEmpty a (Stack a) deriving (Eq, Show, Foldable)

push :: a -> Stack a -> Stack a
push = NonEmpty

pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (NonEmpty a s) = Just (a, s)

peek :: Stack a -> Maybe a
peek = fmap fst . pop

stackSize :: Stack a -> Int
stackSize = getSum . foldMap (const 1)

newtype Crate = Crate {name :: Char} deriving (Eq, Show)

newtype Cargo a = Cargo
  { stacks :: M.IntMap (Stack a)
  }
  deriving (Eq, Show)

peeks :: Cargo a -> [a]
peeks = mapMaybe peek . M.elems . stacks

data Move = Move {from :: Int, to :: Int} deriving (Eq, Show)

data Instruction = Instruction Int Int Int deriving (Eq, Show)

explode :: Instruction -> [Move]
explode (Instruction n f t) = replicate n (Move f t)

explodeAll :: [Instruction] -> [Move]
explodeAll = (=<<) explode

cargoPop :: Int -> Cargo a -> Maybe (a, Cargo a)
cargoPop k c = do
  s <- M.lookup k (stacks c)
  (a, s') <- pop s
  pure (a, update k s' c)
 where
  update :: Int -> Stack a -> Cargo a -> Cargo a
  update k1 s1 = Cargo . insertOrDelete k1 s1 . stacks
  insertOrDelete :: Int -> Stack a -> M.IntMap (Stack a) -> M.IntMap (Stack a)
  insertOrDelete k1 Empty = M.delete k1
  insertOrDelete k1 s1@(NonEmpty _ _) = M.insert k1 s1

cargoPush :: Int -> a -> Cargo a -> Cargo a
cargoPush k a = Cargo . M.alter (insertOrAppend a) k . stacks
 where
  insertOrAppend :: a -> Maybe (Stack a) -> Maybe (Stack a)
  insertOrAppend a1 = Just . push a1 . fromMaybe Empty

rearrange :: Move -> Cargo a -> Cargo a
rearrange (Move k1 k2) c = fromMaybe c $ do
  (a, c') <- cargoPop k1 c
  pure $ cargoPush k2 a c'

rearrangeAll :: [Move] -> Cargo a -> Cargo a
rearrangeAll = appEndo . foldMap (Endo . rearrange)

parseInstruction :: String -> Maybe Instruction
parseInstruction input = case words input of
  ["move", x, "from", y, "to", z] -> do
    x' <- readMay x
    y' <- readMay y
    z' <- readMay z
    pure (Instruction x' y' z')
  _ -> Nothing

parseInput :: T.Text -> Maybe (Cargo Crate, [Instruction])
parseInput input = case T.splitOn "\n\n" input of
   [p1, p2] -> (,) <$> parseCargo p1  <*> Just (parseInstructions p2)
   _ -> Nothing

parseInstructions :: T.Text -> [Instruction]
parseInstructions = mapMaybe (parseInstruction . T.unpack) . T.lines

type Parser a = Parsec Void T.Text a

crateP :: Parser Crate
crateP = Crate <$> between (char '[') (char ']') letterChar

optionalCrateP :: Parser (Maybe Crate)
optionalCrateP = (Just <$> crateP) <|> emptyP where
   emptyP = count 3 spaceChar $> Nothing

cratesP :: Parser [Maybe Crate]
cratesP = optionalCrateP `sepBy` spaceChar

allCratesP :: Parser [[Maybe Crate]]
allCratesP = cratesP `sepEndBy` eol

parseCrates = parse allCratesP ""

parseCargo :: T.Text -> Maybe (Cargo Crate)
parseCargo _ = Just $ Cargo $ M.empty
                      -- takeWhile (not . null) . iterate (drop 4) . tail .
                      --fmap (dropWhile (== ' ') . trasponse . fmap T.unpack . init . T.lines
