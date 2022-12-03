module Main where

import Paths_day03
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set


data Item = Item Char
  deriving (Show, Eq, Ord)

data Backpack = Backpack (Set Item) (Set Item)
  deriving (Show, Eq)

parseBackpack :: String -> Backpack
parseBackpack xs = Backpack (Set.fromList . take (n `div` 2) $ xs') (Set.fromList . drop (n `div` 2) $ xs')
  where
    n = length xs
    xs' = map Item xs

readInput :: IO [Backpack]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . map parseBackpack . lines $ content

priority :: Item -> Int
priority (Item c)
  | Char.isLower c = 1 + Char.ord c - Char.ord 'a'
  | otherwise = 27 + Char.ord c - Char.ord 'A'

commonItemsInCompartments :: Backpack -> Set Item
commonItemsInCompartments (Backpack xs ys) = xs `Set.intersection` ys

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . sum . map priority . concatMap (Set.toList . commonItemsInCompartments) $ input

groupsOf :: [a] -> Int -> [[a]]
groupsOf [] _ = []
groupsOf xs n = take 3 xs : drop 3 xs `groupsOf` n

allItemsInBackpack :: Backpack -> Set Item
allItemsInBackpack (Backpack xs ys) = xs `Set.union` ys

commonItemsInBackpacks :: [Backpack] -> Set Item
commonItemsInBackpacks = foldl1 Set.intersection . map allItemsInBackpack

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . sum . map (priority . head . Set.toList . commonItemsInBackpacks) . (`groupsOf` 3) $ input

main :: IO ()
main = do
  part1 -- prints `Part1: 8039`
  part2 -- prints `Part2: 2510`
