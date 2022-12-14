{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Paths_day14
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import Data.Vector.V2 (V2 (..))
import qualified Data.Vector.V2 as V2
import Data.Chart (Chart)
import qualified Data.Chart as Chart

parser :: ReadP [[V2 Int]]
parser = lineParser `ReadP.sepBy` ReadP.char '\n' <* ReadP.munch (== '\n')
  where
    lineParser = pointParser `ReadP.sepBy1` ReadP.string " -> "
    pointParser = V2 <$> ReadP.nat <*> (ReadP.char ',' *> ReadP.nat)

data State = State
  { chart :: Chart Char
  , dimensions :: (V2 Int, V2 Int)
  } deriving (Show, Eq, Ord)

state0 :: [[V2 Int]] -> State
state0 lines = State { chart, dimensions = Chart.dimensions chart }
  where
    chart = Chart.fromList . map (,'#') . concatMap points . concatMap segments $ lines
    segments (x:y:xs) = (x, y) : segments (y:xs)
    segments _        = []
    points (V2 x0 y0, V2 x1 y1)
      | x0 == x1 = [V2 x0 y | y <- [min y0 y1 .. max y0 y1]]
      | y0 == y1 = [V2 x y0 | x <- [min x0 x1 .. max x0 x1]]

readInput :: IO State
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . state0 . Maybe.fromJust . ReadP.unambiguosParser parser $ content

run :: (State -> Maybe State) -> State -> [State]
run rule = run' . Just
  where
    run' Nothing = []
    run' (Just state) = state : run' (rule state)

data Update = Fall (V2 Int) | Rest | Done
  deriving (Show, Eq, Ord)

eval :: (State -> V2 Int -> Update) -> State -> [State]
eval rule state@State { chart } = eval' (V2 500 0)
  where
    eval' p = case rule state p of
      Fall p' -> eval' p'
      Rest -> state : eval rule (state { chart = Chart.insert p 'o' chart })
      Done -> [state]

fall :: State -> V2 Int -> Maybe (V2 Int)
fall State { chart } (V2 x y) = Maybe.listToMaybe $ filter (not . (`Chart.member` chart)) [V2 x (y+1), V2 (x-1) (y+1), V2 (x+1) (y+1)]

bottomlessRule :: State -> V2 Int -> Update
bottomlessRule state@State { dimensions = (_, V2 _ maxY) } (V2 x y)
  | y > maxY = Done
  | otherwise = case fall state (V2 x y) of
      Just p -> Fall p
      Nothing -> Rest

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . length . filter ((== 'o') . snd) . Chart.toList . chart . last . eval bottomlessRule $ input

bottomfulRule :: State -> V2 Int -> Update
bottomfulRule state@State { chart, dimensions = (_, V2 _ maxY) } (V2 x y)
  | (V2 500 0) `Chart.member` chart = Done
  | y == maxY+1 = Rest
  | otherwise = case fall state (V2 x y) of
      Just p -> Fall p
      Nothing -> Rest

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  -- print . length . filter ((== 'o') . snd) . Chart.toList . chart . last . run bottomful $ input
  print . length . filter ((== 'o') . snd) . Chart.toList . chart . last . eval bottomfulRule $ input

main :: IO ()
main = do
  part1 -- prints `Part1: 1001`
  part2 -- prints `Part2: 27976`
