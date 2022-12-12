{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Graph.Search
  ( VisitedNode (..)
  , Step (..)
  , aStar
  , depthFirst
  ) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as HashPSQ
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet


depthFirst :: (Eq node, Hashable node) => (node -> [node]) -> node -> [node]
depthFirst steps start = worker HashSet.empty [start]
  where
    worker visited [] = []
    worker visited (x:xs)
      | x `HashSet.member` visited = worker visited xs
      | otherwise = x : worker (x `HashSet.insert` visited) (steps x ++ xs)

data VisitedNode node cost = VisitedNode
  { node :: node
  , cameFrom :: node
  , accumulatedCost :: !cost
  } deriving (Show, Eq, Ord, Generic)

instance (Hashable node, Hashable cost) => Hashable (VisitedNode node cost)

data Step node cost = Step
  { next :: node
  , cost :: !cost
  , estimatedRemaingCost :: !cost
  } deriving (Show, Eq, Ord, Generic)

instance (Hashable node, Hashable cost) => Hashable (Step node cost)

aStar :: (Hashable node, Hashable cost, Ord node, Ord cost, Num cost) => (node -> [Step node cost]) -> [node] -> [VisitedNode node cost]
aStar steps starters = worker initialCandidates (HashSet.fromList starters)
  where
    initialCandidates = HashPSQ.fromList
      [ (VisitedNode next start cost, cost + estimatedRemaingCost, ())
      | start <- starters
      , Step next cost estimatedRemaingCost <- steps start
      ]
    worker candidates visited = case HashPSQ.minView candidates of
      Nothing -> []
      Just (visitedNode@VisitedNode { node }, _, _, candidates') -> next
        where
          next
            | node `HashSet.member` visited = worker candidates' visited
            | otherwise = visitedNode : worker candidates'' visited'
          candidates'' = foldr (\step -> HashPSQ.insert (candidate visitedNode step) (priority visitedNode step) ()) candidates' $ steps node
          visited' = node `HashSet.insert` visited

candidate :: Num cost => VisitedNode node cost -> Step node cost -> VisitedNode node cost
candidate VisitedNode { node, accumulatedCost } Step { next, cost } = VisitedNode
  { node = next
  , cameFrom = node
  , accumulatedCost = accumulatedCost + cost
  }

priority :: Num cost => VisitedNode node cost -> Step node cost -> cost
priority VisitedNode { accumulatedCost } Step { cost, estimatedRemaingCost } = accumulatedCost + cost + estimatedRemaingCost

