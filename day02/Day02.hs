{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day02
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe


data Opponent = A | B | C
  deriving (Show, Eq, Ord)

opponentParser :: ReadP Opponent
opponentParser = foldl1 (ReadP.<++)
  [ pure A <* ReadP.char 'A'
  , pure B <* ReadP.char 'B'
  , pure C <* ReadP.char 'C'
  ]

data You = X | Y | Z
  deriving (Show, Eq, Ord)

youParser :: ReadP You
youParser = foldl1 (ReadP.<++)
  [ pure X <* ReadP.char 'X'
  , pure Y <* ReadP.char 'Y'
  , pure Z <* ReadP.char 'Z'
  ]

data Strategy = Strategy
  { opponent :: Opponent
  , you :: You
  }
  deriving (Show, Eq, Ord)

strategyParser :: ReadP Strategy
strategyParser = Strategy <$> opponentParser <*> (ReadP.char ' ' *> youParser)

parser :: ReadP [Strategy]
parser = (strategyParser `ReadP.sepBy` ReadP.char '\n') <* ReadP.munch (== '\n')

readInput :: IO [Strategy]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

data RockPaperScissors = Rock | Paper | Scissors
  deriving (Show, Eq, Ord)

data Round = Round
  { opponentIsPlaying :: RockPaperScissors
  , youArePlaying :: RockPaperScissors
  }
  deriving (Show, Eq, Ord)

data Outcome = Win | Draw | Lose
  deriving (Show, Eq, Ord)

outcome :: Round -> Outcome
outcome Round { opponentIsPlaying, youArePlaying } = eval opponentIsPlaying youArePlaying
  where
    eval Rock Rock = Draw
    eval Rock Paper = Win
    eval Rock Scissors = Lose
    eval Paper Rock = Lose
    eval Paper Paper = Draw
    eval Paper Scissors = Win
    eval Scissors Rock = Win
    eval Scissors Paper = Lose
    eval Scissors Scissors = Draw

outcomePoints :: Outcome -> Int
outcomePoints Win = 6
outcomePoints Draw = 3
outcomePoints Lose = 0

usagePoints :: RockPaperScissors -> Int
usagePoints Rock = 1
usagePoints Paper = 2
usagePoints Scissors = 3

points :: Round -> Int
points round@Round { opponentIsPlaying, youArePlaying } = (outcomePoints . outcome $ round) + usagePoints youArePlaying

fromOpponent :: Opponent -> RockPaperScissors
fromOpponent A = Rock
fromOpponent B = Paper
fromOpponent C = Scissors

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . sum . map (points . fromStrategy) $ input
  where
    fromYou X = Rock
    fromYou Y = Paper
    fromYou Z = Scissors
    fromStrategy (Strategy opponent you) = Round (fromOpponent opponent) (fromYou you)

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . sum . map (points . fromStrategy) $ input
  where
    fromStrategy (Strategy opponent you) = let o = fromOpponent opponent in Round o (deduce o (fromYou you))
    
    fromYou X = Lose
    fromYou Y = Draw
    fromYou Z = Win

    deduce x Draw = x
    deduce Rock Win = Paper
    deduce Paper Win = Scissors
    deduce Scissors Win = Rock
    deduce Rock Lose = Scissors
    deduce Paper Lose = Rock
    deduce Scissors Lose = Paper

main :: IO ()
main = do
  part1 -- prints `Part1: 10941`
  part2 -- prints `Part2: 13071`
