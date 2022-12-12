{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day12
import qualified Data.Char as Char
import Data.Graph.Search (Step (..), VisitedNode (..))
import qualified Data.Graph.Search as Search
import Data.Chart (Chart)
import qualified Data.Chart as Chart
import Data.Vector.V2 (V2 (..))
import qualified Data.Vector.V2 as V2


data HeightMap = HeightMap
  { chart :: Chart Char
  , startPosition :: V2 Int
  , endPosition :: V2 Int
  } deriving (Show, Eq, Ord)

readInput :: IO HeightMap
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  let chart = Chart.fromString content
      startPosition = fst . head . filter ((== 'S') . snd) . Chart.toList $ chart
      endPosition = fst . head . filter ((== 'E') . snd) . Chart.toList $ chart
  return HeightMap { chart, startPosition, endPosition }

height :: HeightMap -> V2 Int -> Int
height HeightMap { chart, startPosition, endPosition } node
  | node == startPosition = 0
  | node == endPosition = Char.ord 'z' - Char.ord 'a'
  | otherwise =  (Char.ord $ chart Chart.! node) - (Char.ord 'a')

steps :: HeightMap -> V2 Int -> [Step (V2 Int) Int]
steps heightMap@HeightMap { chart, endPosition } node =
  [ Step node' 1 (V2.manhattan $ node' - endPosition)
  | node' <- map (node+) [Chart.north, Chart.south, Chart.west, Chart.east]
  , node' `Chart.member` chart
  , heightMap `height` node' - heightMap `height` node <= 1
  ]

part1 :: IO ()
part1 = do
  heightMap@HeightMap { startPosition, endPosition } <- readInput
  putStr "Part1: "
  let search = Search.aStar (steps heightMap) [startPosition]
  print . Search.accumulatedCost . head . dropWhile ((/= endPosition) . Search.node) $ search

part2 :: IO ()
part2 = do
  heightMap@HeightMap { chart, endPosition } <- readInput
  putStr "Part2: "
  let search = Search.aStar (steps heightMap) . map fst . filter ((== 'a') . snd) . Chart.toList $ chart
  print . Search.accumulatedCost . head . dropWhile ((/= endPosition) . Search.node) $ search

main :: IO ()
main = do
  part1 -- prints `Part1: 447`
  part2 -- prints `Part2: 446`
