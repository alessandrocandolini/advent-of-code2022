module Day9 where

import Data.Either (fromRight)
import Data.Functor (($>))
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void ( Void )
import Text.Megaparsec ( (<|>), parse, Parsec, ParseErrorBundle )
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer
  { solution1 :: Int
  , solution2 :: Int
  }
  deriving (Eq, Show)

logic :: T.Text -> Answer
logic = (Answer <$> solve rope1 <*> solve rope2) . explodeInstructions . parseInstructions

solve :: Rope -> [Direction] -> Int
solve initialRope = length . nub . fmap lastKnot . evolveRope initialRope
 where
  lastKnot :: Rope -> Knot
  lastKnot = N.last . knots

rope1 :: Rope
rope1 = generateRope 2

rope2 :: Rope
rope2 = generateRope 10

generateRope :: Int -> Rope
generateRope = Rope . replicateNonEmpty knot
 where
  replicateNonEmpty :: a -> Int -> NonEmpty a
  replicateNonEmpty a n = a N.:| replicate (n - 1) a

knot :: Knot
knot = Knot 0 0

newtype Rope = Rope
  { knots :: NonEmpty Knot
  }
  deriving (Eq, Show)

data Knot = Knot Int Int deriving (Eq, Show)

data Direction = Up | Down | LeftWard | RightWard deriving (Eq, Show)

move :: Direction -> Knot -> Knot
move Up (Knot x y) = Knot x (y + 1)
move Down (Knot x y) = Knot x (y - 1)
move LeftWard (Knot x y) = Knot (x - 1) y
move RightWard (Knot x y) = Knot (x + 1) y

data Proximity = Close | Distant deriving (Eq, Show)

chebyshev :: Knot -> Knot -> Int
chebyshev (Knot x1 y1) (Knot x2 y2) = max (abs (x1 - x2)) (abs (y1 - y2))

proximity :: Knot -> Knot -> Proximity
proximity target current
  | chebyshev target current <= 1 = Close
  | otherwise = Distant

moveTowards :: Knot -> Knot -> Knot
moveTowards target current | proximity target current == Close = current
moveTowards (Knot xH yH) (Knot xT yT) = Knot (xT + signum (xH - xT)) (yT + signum (yH - yT))

moveRope :: Rope -> Direction -> Rope
moveRope (Rope (leading N.:| others)) direction = Rope (leading' N.:| others')
 where
  leading' = move direction leading
  others' = zipWith moveTowards (leading' : others') others

evolveRope :: Rope -> [Direction] -> [Rope]
evolveRope = scanl moveRope

data Instruction = Instruction Direction Int deriving (Eq, Show)

explodeInstruction :: Instruction -> [Direction]
explodeInstruction (Instruction direction times) = replicate times direction

explodeInstructions :: [Instruction] -> [Direction]
explodeInstructions = (=<<) explodeInstruction

type Parser = Parsec Void T.Text

directionP :: Parser Direction
directionP =
  char 'R'
    $> RightWard
    <|> char 'L'
    $> LeftWard
    <|> char 'U'
    $> Up
    <|> char 'D'
    $> Down

instructionP :: Parser Instruction
instructionP = Instruction <$> (directionP <* char ' ') <*> decimal

parseInstruction :: T.Text -> Either (ParseErrorBundle T.Text Void) Instruction
parseInstruction = parse instructionP ""

parseInstructions :: T.Text -> [Instruction]
parseInstructions = fromRight [] . traverse parseInstruction . T.lines
