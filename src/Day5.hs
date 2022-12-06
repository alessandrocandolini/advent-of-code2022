{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}

module Day5 where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap pureProgram . T.readFile

data Report = Report deriving (Eq, Show)

pureProgram :: T.Text -> Report
pureProgram = const Report

data Stack a = Empty | NonEmpty a (Stack a) deriving (Eq, Show)

push :: a -> Stack a -> Stack a
push = NonEmpty

pop :: Stack a -> Maybe (a, Stack a)
pop Empty = Nothing
pop (NonEmpty a s) = Just (a, s)

stackSize :: Stack a -> Int
stackSize Empty = 0
stackSize (NonEmpty _ s) = 1 + stackSize s

newtype Crate = Crate String deriving (Eq, Show)

newtype Cargo = Cargo {
     stacks :: V.Vector (Stack Crate) } deriving (Eq, Show)

cargoSize :: Cargo -> Int
cargoSize = V.length . stacks

content :: Cargo -> Int
content = sum . fmap stackSize . stacks

newtype Origin = Origin Int deriving (Eq, Show, Num, Ord) via Int

newtype Destination = Destination Int deriving (Eq, Show, Num, Ord) via Int

data Move = Move Origin Destination deriving (Eq, Show)

reverseMove :: Move -> Move
reverseMove (Move (Origin i) (Destination j)) = Move (Origin j) (Destination i)

cargoPop :: Cargo -> Int -> Maybe (Crate, Cargo)
cargoPop c i = do
     s <- (stacks c) V.!? i
     (a, s') <- pop s
     pure (a, Cargo $ (stacks c) V.// [(i, s')])

cargoPush :: Cargo -> Int -> Crate -> Cargo
cargoPush c _ _ = c
--cargoPush c i a = let
        --s' = case stacks c V.!? i of
          --Just s ->  push a s
          --Nothing -> push a Empty
       --in
        --Cargo $ V.update (stacks c) (V.singleton (i, s'))



rearrange :: Move -> Cargo -> Cargo
rearrange (Move (Origin o) (Destination d)) c = maybe c (\(a, c') -> cargoPush c' d a) (cargoPop c o)
