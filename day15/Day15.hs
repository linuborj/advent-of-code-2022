{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day15
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import Data.Vector.V2 (V2 (..))
import qualified Data.Vector.V2 as V2


data DetectedBeacon = DetectedBeacon
  { sensorPosition :: V2 Int
  , beaconPosition :: V2 Int
  } deriving (Show, Eq, Ord)

detectedBeaconParser :: ReadP DetectedBeacon
detectedBeaconParser = do
  ReadP.string "Sensor at x="
  sensorX <- ReadP.int
  ReadP.string ", y="
  sensorY <- ReadP.int
  ReadP.string ": closest beacon is at x="
  beaconX <- ReadP.int
  ReadP.string ", y="
  beaconY <- ReadP.int
  return DetectedBeacon
    { sensorPosition = V2 sensorX sensorY
    , beaconPosition = V2 beaconX beaconY
    }

parser :: ReadP [DetectedBeacon]
parser = detectedBeaconParser `ReadP.sepBy` ReadP.char '\n' <* ReadP.munch (== '\n')

readInput :: IO [DetectedBeacon]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

merge :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
merge (l, r) [] = [(l, r)]
merge (l, r) ((l', r'):rs)
  | l <= l' && r >= r' = merge (l, r) rs
  | l >= l' && r <= r' = (l', r'):rs
  | l <= l' && r >= l' = merge (l, r') rs
  | l <= r' && r >= r' = merge (l', r) rs
  | otherwise          = (l', r') : merge (l, r) rs

intervals :: Int -> DetectedBeacon -> [(Int, Int)]
intervals y DetectedBeacon { sensorPosition, beaconPosition }
  | d > r = []
  | otherwise = [(V2.x sensorPosition - r + d, V2.x sensorPosition + r - d)]
  where
    d = abs $ y - V2.y sensorPosition 
    r = V2.manhattan $ sensorPosition - beaconPosition

unavailable :: Int -> [DetectedBeacon] -> [(Int, Int)]
unavailable y = foldl (flip merge) [] . concatMap (intervals y)

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . (\(l, r) -> r - l) . head .  unavailable 2000000 $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . head $ [ y + x * 4000000
                 | y <- [0..4000000]
                 , let is = unavailable y input
                 , length is > 1
                 , let x = snd (head is) + 1
                 ]

main :: IO ()
main = do
  part1 -- prints `Part1: 5176944`
  part2 -- prints `Part2: 13350458933732`
