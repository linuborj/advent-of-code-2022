module Main where

import Paths_day01
import qualified Data.List as List


splitOn :: Eq a => [a] -> a -> [[a]]
splitOn [] _ = []
splitOn xs y = splitOn' [] xs y
  where
    splitOn' acc [] _ = [acc]
    splitOn' acc (x:xs) y
      | x == y = acc : splitOn' [] xs y
      | otherwise = splitOn' (x:acc) xs y

readInput :: IO [[Int]]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . map (map read) . (`splitOn` "") . lines $ content

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . maximum . map sum $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . sum . take 3 . reverse . List.sort . map sum $ input

main :: IO ()
main = do
  part1 -- prints `Part1: 69501`
  part2 -- prints `Part2: 202346`
