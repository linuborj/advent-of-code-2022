{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Paths_day16
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Graph.Search as Search
import qualified Data.List as List
import qualified Control.Monad as Monad
import qualified Data.Char as Char


data Room = Room
  { valve :: String
  , flowRate :: Int
  , tunnels :: [String]
  } deriving (Show, Eq, Ord)

newtype TunnelSystem  = TunnelSystem { rooms :: [Room] }
  deriving (Show, Eq, Ord)

parseRoom :: ReadP Room
parseRoom = do
  ReadP.string "Valve "
  valve <- parseValve
  ReadP.string " has flow rate="
  flowRate <- ReadP.nat
  ReadP.string "; "
  ReadP.string "tunnels lead to valves" ReadP.<++ ReadP.string "tunnel leads to valve"
  ReadP.string " "
  tunnels <- parseValve `ReadP.sepBy` ReadP.string ", "
  return Room { valve, flowRate, tunnels  }
  where
    parseValve = Monad.replicateM 2 (ReadP.satisfy Char.isUpper)

parser :: ReadP TunnelSystem
parser = TunnelSystem <$> (parseRoom `ReadP.sepBy` ReadP.char '\n' <* ReadP.munch (== '\n'))

data Context = Context
  { intactValves :: HashSet String
  , distances :: (String, String) -> Int
  , flowRates :: String -> Int
  }

fromTunnelSystem :: TunnelSystem -> Context
fromTunnelSystem TunnelSystem { rooms } = Context
  { intactValves = HashSet.fromList [valve room | room <- rooms, flowRate room > 0 ]
  , distances = Search.floydWarshall [valve room | room <- rooms] (nodes HashMap.!)
  , flowRates = (flow HashMap.!)
  }
  where
    nodes = HashMap.fromList [(valve room, map (,1) $ tunnels room) | room <- rooms]
    flow = HashMap.fromList [(valve room, flowRate room) | room <- rooms]

readInput :: IO TunnelSystem
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

data Node = Node
  { position :: String
  , remainingTime :: Int
  , openedValves :: HashSet String
  , releasedPressure :: Int
  } deriving (Eq, Ord, Show, Generic)

instance Hashable Node

unOpenedValves :: Context -> Node -> [String]
unOpenedValves Context { intactValves } node
  = HashSet.toList
  $ intactValves `HashSet.difference` (openedValves node)

startNode :: Int -> Node
startNode remainingTime = Node
  { position = "AA"
  , remainingTime
  , openedValves = HashSet.empty
  , releasedPressure = 0
  }

neighbors :: Context -> Node -> [Node]
neighbors context@Context { distances, flowRates } node@Node { position, remainingTime, openedValves, releasedPressure }
  | remainingTime <= 0 = []
  | otherwise = continue
  where
    continue =
      [ Node position' remainingTime' openedValves' releasedPressure'
      | position' <- unOpenedValves context node
      , let remainingTime' = remainingTime - distances (position, position') - 1
            openedValves' = position' `HashSet.insert` openedValves
            releasedPressure' = releasedPressure + remainingTime' * flowRates position'
      , remainingTime > 0
      ] ++ [Node position 0 openedValves releasedPressure]

explore :: TunnelSystem -> Node -> [Node]
explore tunnelSystem startNode = Search.depthFirst (neighbors context) startNode
  where
     context = fromTunnelSystem tunnelSystem

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . maximum . map releasedPressure . filter ((== 0) . remainingTime) . explore input $ startNode 30

part2 :: IO ()
part2 = do
  input <- readInput
  let scores = HashMap.fromListWith max . map (\x -> (openedValves x, releasedPressure x))  . filter ((== 0) . remainingTime) . explore input $ startNode 26
      keys = HashMap.keys scores
      disjoint i j = HashSet.null $ i `HashSet.intersection` j
      score i j = sum $ map (scores HashMap.!) [i, j]
  putStr "Part2: "
  print . maximum $ [score i j | i <- keys, j <- reverse keys, i `disjoint` j]

main :: IO ()
main = do
  part1 -- prints `Part1: 2250`
  part2 -- prints `Part2: 3015`

