{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day04
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe


data Range = Range Int Int
  deriving (Eq, Ord)

instance Show Range where
  show (Range from to) = show from ++ "-" ++ show to

rangeParser :: ReadP Range
rangeParser = Range <$> ReadP.int <*> (ReadP.char '-' *> ReadP.int)

rangePairParser :: ReadP (Range, Range)
rangePairParser = (,) <$> rangeParser <*> (ReadP.char ',' *> rangeParser)

parser :: ReadP [(Range, Range)]
parser = rangePairParser `ReadP.sepBy` (ReadP.char '\n') <* ReadP.munch (== '\n')

readInput :: IO [(Range, Range)]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

isContainedBy :: Range -> Range -> Bool
isContainedBy (Range x y) (Range z w) = z <= x && w >= y

completelyOverlaps :: Range -> Range -> Bool
completelyOverlaps x y = x `isContainedBy` y || y `isContainedBy` x

partiallyOverlaps :: Range -> Range -> Bool
partiallyOverlaps (Range x y) (Range z w) = (x <= z && z <= y) || (x <= w && w <= y)

overlaps :: Range -> Range -> Bool
overlaps x y = x `partiallyOverlaps` y || x `completelyOverlaps` y

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . length . filter (uncurry completelyOverlaps) $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . length . filter (uncurry overlaps) $ input

main :: IO ()
main = do
  part1 -- prints `Part1: 524`
  part2 -- prints `Part2: 798`
