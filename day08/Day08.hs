module Main where

import Paths_day08
import Data.Chart (Chart)
import qualified Data.Chart as Chart
import Data.Vector.V2 (V2 (..))
import qualified Data.Vector.V2 as V2
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List


readInput :: IO (Chart Int)
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . fmap (read . (:[])) . Chart.fromString $ content

visible :: Chart Int -> [V2 Int] -> Set (V2 Int)
visible chart (x:xs) = Set.fromList $ x : worker (chart Chart.! x) xs
  where
    worker n [] = []
    worker n (x:xs)
      | chart Chart.! x > n = x : worker (chart Chart.! x) xs
      | otherwise = worker n xs

rows :: Chart a ->  [[V2 Int]]
rows chart = [ [ V2 x y | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where
    (V2 xMin yMin, V2 xMax yMax) = Chart.dimensions chart

columns :: Chart a -> [[V2 Int]]
columns = List.transpose . rows

sightlines :: Chart a -> [[V2 Int]]
sightlines chart = concat
  [ rows chart
  , map reverse $ rows chart
  , columns chart
  , map reverse $ columns chart
  ]

notBlocking :: Chart Int -> V2 Int -> [V2 Int] -> Int
notBlocking chart x xs = worker xs
  where
    n = chart Chart.! x
    worker [] = 0
    worker (x:xs)
      | chart Chart.! x < n = 1 + worker xs
      | otherwise = 1

scenicScore :: Chart Int -> V2 Int -> Int
scenicScore chart base@(V2 x y) = product . map (notBlocking chart base) $ [west, east, north, south]
  where
    (V2 xMin yMin, V2 xMax yMax) = Chart.dimensions chart
    west = [ V2 x' y | x' <- reverse [xMin..x-1] ]
    east = [ V2 x' y | x' <- [x+1..xMax] ]
    north = [ V2 x y' | y' <- reverse [yMin..y-1] ]
    south = [ V2 x y' | y' <- [y+1..yMax] ]

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . Set.size . foldl1 Set.union . map (visible input) . sightlines $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . maximum . map (scenicScore input) . Chart.keys $ input

main :: IO ()
main = do
  part1 -- prints `Part1: `
  part2 -- prints `Part2: `
