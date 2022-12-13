module Main where

import Paths_day13
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import qualified Data.List as List


data Term
  = V Int
  | L [Term]
  deriving (Eq, Read, Show)

parseTerm :: ReadP Term
parseTerm = foldl1 (ReadP.<++)
  [ V <$> ReadP.nat
  , L <$> (ReadP.char '[' *> parseTerm `ReadP.sepBy` ReadP.char ',' <* ReadP.char ']')
  ]

parser :: ReadP [(Term, Term)]
parser = do
  pairs <- pairParser `ReadP.sepBy` ReadP.string "\n\n"
  ReadP.munch (== '\n')
  return pairs
  where
    pairParser = do
      x <- parseTerm
      ReadP.char '\n'
      y <- parseTerm
      return (x, y)

readInput :: IO [(Term, Term)]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

compareTerms :: Term -> Term -> Ordering
compareTerms (V x)      (V y)      = compare x y
compareTerms (L (x:xs)) (L (y:ys)) = compareTerms x y <> compareTerms (L xs) (L ys)
compareTerms (L [])     (L [])     = EQ
compareTerms (L [])     (L (y:ys)) = LT
compareTerms (L (x:xs)) (L [])     = GT
compareTerms (V x)      (L ys)     = compareTerms (L [V x]) (L ys)
compareTerms (L xs)     (V y)      = compareTerms (L xs) (L [V y])

inRightOrder :: (Term, Term) -> Bool
inRightOrder (x, y) = x `compareTerms` y == LT

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . sum . map fst . filter (inRightOrder . snd) . zip [1..] $ input

flatten :: [(Term, Term)] -> [Term]
flatten = concatMap (\(x, y) -> [x, y])

dividerPackets :: [Term]
dividerPackets = [L [L [V 2]], L [L [V 6]]]

decoderKey :: [Term] -> Int
decoderKey = product . findDividerPacketIndices . List.sortBy compareTerms . (dividerPackets ++)
  where
    findDividerPacketIndices = map fst . filter ((`elem` dividerPackets) . snd) . zip [1..]

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . decoderKey . flatten $ input

main :: IO ()
main = do
  part1 -- prints `Part1: 5806`
  part2 -- prints `Part2: 23600`
