module Main where

import Paths_day06
import qualified Data.Set as Set


readInput :: IO String
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return content

markerPosition :: Int -> String -> Int
markerPosition n xs
  | n == Set.size (Set.fromList $ take n xs) = n
  | otherwise = 1 + markerPosition n (tail xs)

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . markerPosition 4 $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . markerPosition 14 $ input

main :: IO ()
main = do
  part1 -- prints `Part1: 1598`
  part2 -- prints `Part2: 2414`
