module Main where

import Paths_dayX

readInput :: IO String
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return content

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "

main :: IO ()
main = do
  part1 -- prints `Part1: `
  part2 -- prints `Part2: `
