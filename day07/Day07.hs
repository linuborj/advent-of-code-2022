{-# LANGUAGE NamedFieldPuns #-}
module Main where

import Paths_day07
import Prelude hiding (FilePath (..))
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List


nameParser :: ReadP String
nameParser = ReadP.many1 . ReadP.satisfy $ not . Char.isSpace

data Output = DirectoryListing String | FileListing Int String
  deriving (Eq, Ord)

instance Show Output where
  show (FileListing size name) = show size <> " " <> name
  show (DirectoryListing name) = "dir " <> name

outputParser :: ReadP Output
outputParser = foldl1 (ReadP.<++)
  [ FileListing <$> ReadP.nat <*> (ReadP.string " " *> nameParser <* ReadP.char '\n')
  , DirectoryListing <$> (ReadP.string "dir " *> nameParser <* ReadP.char '\n')
  ]

data Execution = Cd String | Ls [Output]
  deriving (Eq, Ord)

instance Show Execution where
  show (Cd directory) = "$ cd " <> directory
  show (Ls outputs) = concat . List.intersperse "\n" $ "$ ls" : map show outputs

executionParser :: ReadP Execution
executionParser = foldl1 (ReadP.<++)
  [ Cd <$> (ReadP.string "$ cd " *> nameParser <* ReadP.char '\n')
  , Ls <$> (ReadP.string "$ ls\n" *> ReadP.many outputParser)
  ]

parser :: ReadP [Execution]
parser = ReadP.many executionParser

readInput :: IO [Execution]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

data Path = Root | Path String Path
  deriving (Show, Eq, Ord)

up :: Path -> Path
up Root = Root
up (Path _ path) = path

cds :: Path -> [String]
cds = reverse . cds'
  where
   cds' Root = []
   cds' (Path directory path) = directory : cds' path

data FilePath = FilePath Path String
  deriving (Show, Eq, Ord)

data File = File { path :: FilePath, size :: Int }
  deriving (Show, Eq, Ord)

interpret :: [Execution] -> [File]
interpret = interpret' Root
  where
    interpret' path []              = []
    interpret' path (Cd "/":xs)     = interpret' Root xs
    interpret' path (Cd "..":xs)    = interpret' (up path) xs
    interpret' path (Cd p:xs)       = interpret' (Path p path) xs
    interpret' path (Ls outputs:xs) = concatMap (files path) outputs ++ interpret' path xs

    files path (FileListing size name) = [File { path = FilePath path name, size }]
    files _ _  = []

data Directory = Directory { files :: Map String File, subDirectories :: Map String Directory, directorySize :: Int }
  deriving (Show, Eq, Ord)

emptyDirectory :: Directory
emptyDirectory = Directory { files = Map.empty, subDirectories = Map.empty, directorySize = 0}

insertSubDirectory :: String -> Directory -> Directory -> Directory
insertSubDirectory name subDir into@Directory { subDirectories } = into
  { subDirectories = Map.insert name subDir subDirectories
  , directorySize = directorySize into + directorySize subDir
  }

insertFile :: File -> Directory -> Directory
insertFile file@File { path = FilePath _ fileName, size } into@Directory { files, directorySize } = into
  { files = Map.insert fileName file files
  , directorySize = directorySize + size
  }

mergeDirectories :: Directory -> Directory -> Directory
mergeDirectories left right = Directory
  { files = files left `Map.union` files right
  , subDirectories = Map.unionWith mergeDirectories (subDirectories left) (subDirectories right)
  , directorySize = directorySize left + directorySize right
  }

fileTree :: [File] -> Directory
fileTree = foldl1 mergeDirectories . map (\file -> branch file $ location file)
  where
    branch file [] = insertFile file emptyDirectory
    branch file (p:ps) = insertSubDirectory p (branch file ps) emptyDirectory

    location File { path = FilePath path _ } = cds path


allSubDirectories :: Directory -> [Directory]
allSubDirectories dir = dir : concatMap allSubDirectories (Map.elems . subDirectories $ dir)

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . sum . filter (<=100000) . map directorySize . tail . allSubDirectories . fileTree . interpret $ input

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  let fileSystem = fileTree . interpret $ input
  let usedSpace = directorySize fileSystem
  let totalSpace = 70000000
  let neededSpace = 30000000
  let wouldFreeUpEnoughSpace deleted = totalSpace - usedSpace + deleted >= neededSpace
  print . minimum . filter wouldFreeUpEnoughSpace . map directorySize . tail . allSubDirectories $ fileSystem

main :: IO ()
main = do
  part1 -- prints `Part1: 1915606`
  part2 -- prints `Part2: 5025657`
