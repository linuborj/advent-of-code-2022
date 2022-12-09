module Main where

import Paths_day09
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import Data.Vector.V2 (V2 (..))
import qualified Data.Vector.V2 as V2
import Data.Set (Set)
import qualified Data.Set as Set


data Move
  = U Int
  | D Int
  | L Int
  | R Int
  deriving (Show, Eq, Ord)

moveParser :: ReadP Move
moveParser = foldl1 (ReadP.<++)
  [ U <$> (ReadP.string "U " *> ReadP.nat)
  , D <$> (ReadP.string "D " *> ReadP.nat)
  , L <$> (ReadP.string "L " *> ReadP.nat)
  , R <$> (ReadP.string "R " *> ReadP.nat)
  ]

parser :: ReadP [Move]
parser = moveParser `ReadP.sepBy` (ReadP.char '\n') <* ReadP.munch (=='\n')

readInput :: IO [Move]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

steps :: Move -> [V2 Int]
steps (U n) = replicate n (V2 0 (-1))
steps (D n) = replicate n (V2 0 1)
steps (L n) = replicate n (V2 (-1) 0)
steps (R n) = replicate n (V2 1 0)

areAdjacent :: V2 Int -> V2 Int -> Bool
areAdjacent h t = all (<= 1) . abs $ h - t

move :: [V2 Int] -> V2 Int -> [V2 Int]
move rope d = rope'
  where
    rope' = (head rope + d) : zipWith follow rope' (tail rope)
    follow h t
      | areAdjacent h t = t
      | otherwise = t + (signum $ h - t)

tailPositions :: Int -> [Move] -> Set (V2 Int)
tailPositions n = Set.fromList . map last . scanl move (replicate n 0) . concatMap steps

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . Set.size . tailPositions 2 $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . Set.size . tailPositions 10 $ input

main :: IO ()
main = do
  part1 -- prints `Part1: 6037`
  part2 -- prints `Part2: 2485`
