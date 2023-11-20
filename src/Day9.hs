module Day9 where

import Data.Either (fromRight)
import Data.Either.Combinators (rightToMaybe)
import Data.Functor (($>))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int deriving (Eq, Show)

logic :: T.Text -> Answer
logic = Answer . length . nub . fmap ropeTail . evolveRope initialRope . explodeAll . parseInstruction

initialRope :: Rope
initialRope = Rope (Position 0 0) (Position 0 0)

data Position = Position Int Int deriving (Eq, Show)

data Direction = Up | Down | LeftWard | RightWard deriving (Eq, Show)

data Rope = Rope {ropeHead :: Position, ropeTail :: Position} deriving (Eq, Show)

move :: Direction -> Position -> Position
move Up (Position x y) = Position x (y + 1)
move Down (Position x y) = Position x (y - 1)
move LeftWard (Position x y) = Position (x - 1) y
move RightWard (Position x y) = Position (x + 1) y

chebyshev :: Position -> Position -> Int
chebyshev (Position x1 y1) (Position x2 y2) = max (abs (x1 - x2)) (abs (y1 - y2))

moveClose :: Position -> Position -> Position
moveClose h t | chebyshev h t <= 1 = t
moveClose (Position xH yH) (Position xT yT) = Position (xT + signum (xH - xT)) (yT + signum (yH - yT))

moveRope :: Rope -> Direction -> Rope
moveRope (Rope h t) direction = Rope newH newT
 where
  newH = move direction h
  newT = moveClose newH t

evolveRope :: Rope -> [Direction] -> [Rope]
evolveRope = scanl moveRope

data Instruction = Instruction Direction Int deriving (Eq, Show)

explode :: Instruction -> [Direction]
explode (Instruction direction times) = replicate times direction

explodeAll :: [Instruction] -> [Direction]
explodeAll = (=<<) explode

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

parseInstruction :: T.Text -> [Instruction]
parseInstruction = fromRight [] . traverse (parse instructionP "") . T.lines
