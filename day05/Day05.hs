{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day05
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap


newtype Crate = Crate { unCrate :: Char }
  deriving (Show, Eq, Ord)

crateParser :: ReadP Crate
crateParser = Crate <$> (ReadP.char '[' *> ReadP.satisfy Char.isUpper <* ReadP.char ']')

crateOrEmptyParser :: ReadP (Maybe Crate)
crateOrEmptyParser = foldl1 (ReadP.<++)
  [ Just <$> crateParser
  , ReadP.string "   " *> pure Nothing
  ]

rowParser :: ReadP [Maybe Crate]
rowParser = crateOrEmptyParser `ReadP.sepBy` ReadP.char ' '

columnParser :: ReadP [[Crate]]
columnParser = map (concatMap Maybe.maybeToList) . List.transpose <$> (rowParser `ReadP.sepBy` ReadP.char '\n')

identifiersParser :: ReadP [Int]
identifiersParser = (ReadP.char ' ' *> ReadP.nat <* ReadP.char ' ') `ReadP.sepBy` ReadP.char ' '

stackParser :: ReadP [(Int, [Crate])]
stackParser = do
  columns <- columnParser
  ReadP.munch (== '\n')
  identifiers <- identifiersParser
  return $ zip identifiers columns

data Move = Move
  { amount :: Int
  , from :: Int
  , to :: Int
  } deriving (Show, Eq, Ord)

moveParser :: ReadP Move
moveParser = do
  ReadP.string "move "
  amount <- ReadP.nat
  ReadP.string " from "
  from <- ReadP.nat
  ReadP.string " to "
  to <- ReadP.nat
  return Move { amount, from, to }

data Input = Input
  { stacks :: IntMap [Crate]
  , moves :: [Move]
  } deriving (Show, Eq, Ord)

parser :: ReadP Input
parser = do
  stacks <- IntMap.fromList <$> stackParser
  ReadP.munch (== '\n')
  moves <- moveParser `ReadP.sepBy` ReadP.char '\n'
  ReadP.munch (== '\n')
  return Input { stacks, moves }

readInput :: IO Input
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

data MoveType = OneByOne | AllAtOnce
  deriving (Show, Eq, Ord)

executeMove :: MoveType -> IntMap [Crate] -> Move -> IntMap [Crate]
executeMove moveType stacks Move { amount, from, to } = foldr (uncurry IntMap.insert) stacks
  [ (from, fromStack')
  , (to, toStack')
  ]
  where
    fromStack = stacks IntMap.! from
    toStack = stacks IntMap.! to
    cratesMoved = take amount fromStack
    fromStack' = drop amount fromStack
    toStack' = case moveType of
      OneByOne -> reverse cratesMoved ++ toStack
      AllAtOnce ->  cratesMoved ++ toStack

executeMoves :: MoveType -> IntMap [Crate] -> [Move] -> IntMap [Crate]
executeMoves = foldl . executeMove

topOfEachStack :: IntMap [Crate] -> [Crate]
topOfEachStack = map (head . snd) . IntMap.toList

part1 :: IO ()
part1 = do
  Input { stacks, moves } <- readInput
  putStr "Part1: "
  print . map unCrate . topOfEachStack $ executeMoves OneByOne stacks moves

part2 :: IO ()
part2 = do
  Input { stacks, moves } <- readInput
  putStr "Part2: "
  print . map unCrate . topOfEachStack $ executeMoves AllAtOnce stacks moves

main :: IO ()
main = do
  part1 -- prints `Part1: "FWSHSPJWM"`
  part2 -- prints `Part2: "PWPWHGFZS"`
