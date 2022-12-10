module Main where

import Paths_day10
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe


data Instruction
  = Noop
  | Addx Int
  deriving (Show, Eq, Ord)

instructionParser :: ReadP Instruction
instructionParser = foldl1 (ReadP.<++)
  [ pure Noop <* ReadP.string "noop"
  , Addx <$> (ReadP.string "addx " *> ReadP.int)
  ]

parser :: ReadP [Instruction]
parser = instructionParser `ReadP.sepBy` (ReadP.char '\n') <* ReadP.munch (=='\n')

readInput :: IO [Instruction]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

type State = Int

evalInstruction :: State -> Instruction -> [State]
evalInstruction x Noop = [x]
evalInstruction x (Addx dx) = [x, x + dx]

eval :: [Instruction] -> [State]
eval = concat . scanl (evalInstruction . last) [1]

signalStrength :: [State] -> Int
signalStrength xs = sum $ map strength [20, 60, 100, 140, 180, 220]
  where
    strength n = n * (xs !! (n - 1))

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . signalStrength . eval $ input

crt :: [State] -> String
crt = unlines . format . pixels
  where
    pixels = zipWith pixel (concat $ replicate 6 [0..39])
    pixel i x
      | abs (i - x) <= 1 = '#'
      | otherwise = '.'
    format [] = []
    format xs = take 40 xs : format (drop 40 xs)

part2 :: IO ()
part2 = do
  input <- readInput
  putStrLn "Part2: "
  putStrLn . crt . eval $ input

main :: IO ()
main = do
  part1 -- prints `Part1: `
  part2 -- prints `Part2:
        -- ####.#..#.###..###..####.####..##..#....
        -- ...#.#..#.#..#.#..#.#....#....#..#.#....
        -- ..#..#..#.#..#.#..#.###..###..#....#....
        -- .#...#..#.###..###..#....#....#....#....
        -- #....#..#.#....#.#..#....#....#..#.#....
        -- ####..##..#....#..#.#....####..##..####.`

