{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Prelude hiding (id)
import Paths_day11
import Text.ParserCombinators.ReadP.Unambiguous (ReadP)
import qualified Text.ParserCombinators.ReadP.Unambiguous as ReadP
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import Control.Monad (forM_)
import Control.Lens


data Operand
  = Value { _operandValue :: Int }
  | Old
  deriving (Show, Eq, Ord)

makeLenses ''Operand

data Operation
  = Add { _arg0 :: Operand, _arg1 :: Operand }
  | Multiply { _arg0 :: Operand, _arg1 :: Operand }
  deriving (Show, Eq, Ord)

makeLenses ''Operation

data Condition = IsDivisibleBy { _divisor :: Int }
  deriving (Show, Eq, Ord)

makeLenses ''Condition

data Test = Test { _condition :: Condition, _ifTrue :: Int, _ifFalse :: Int }
  deriving (Show, Eq, Ord)

makeLenses ''Test

newtype Id = Id { unId :: Int }
  deriving (Show, Eq, Ord)

data Monkey = Monkey
  { _id :: Id
  , _items :: [Int]
  , _operation :: Operation
  , _test :: Test
  } deriving (Show, Eq, Ord)

makeLenses ''Monkey

parseOperation :: ReadP Operation
parseOperation = do
  ReadP.string "new = "
  x <- operandParser
  ReadP.string " "
  operator <- ReadP.char '*' ReadP.<++ ReadP.char '+'
  ReadP.string " "
  y <- operandParser
  return $ case operator of
    '+' -> Add x y
    '*' -> Multiply x y
  where
    operandParser = foldl1 (ReadP.<++)
      [ pure Old <* ReadP.string "old"
      , Value <$> ReadP.nat
      ]

parseMonkey :: ReadP Monkey
parseMonkey = do
  ReadP.string "Monkey "
  id <- Id <$> ReadP.nat
  ReadP.string ":\n  Starting items: "
  items <- ReadP.nat `ReadP.sepBy` ReadP.string ", "
  ReadP.string "\n  Operation: "
  operation <- parseOperation
  ReadP.string "\n  Test: divisible by "
  condition <- IsDivisibleBy <$> ReadP.nat
  ReadP.string "\n    If true: throw to monkey "
  ifTrue <- ReadP.nat
  ReadP.string "\n    If false: throw to monkey "
  ifFalse <- ReadP.nat
  let test = Test { _condition = condition, _ifTrue = ifTrue, _ifFalse = ifFalse }
  return $ Monkey { _id = id, _items = items, _operation = operation, _test = test }

parser :: ReadP [Monkey]
parser = do
  monkeys <- parseMonkey `ReadP.sepBy` ReadP.munch (== '\n')
  ReadP.munch (== '\n')
  return monkeys

readInput :: IO [Monkey]
readInput = do
  fileName <- getDataFileName "input"
  content <- readFile fileName
  return . Maybe.fromJust . ReadP.unambiguosParser parser $ content

data Env = Env
  { _monkeys :: IntMap Monkey
  , _inspections :: IntMap Int
  , _base :: Int
  } deriving (Show, Eq, Ord)

makeLenses ''Env

moduloBase :: Int -> State Env Int
moduloBase n = use $ base . to (n `mod`)

monkey :: Applicative f => Id -> LensLike' f Env Monkey
monkey id = monkeys . at (unId id) . _Just

takeItemFromMonkey :: Id -> State Env (Maybe Int)
takeItemFromMonkey id = do
  xs <- use $ monkey id . items
  case xs of
    [] -> return Nothing
    (x:xs) -> do
      (monkey id . items) `assign` xs
      return $ Just x

giveItemToMonkey :: Id -> Int -> State Env ()
giveItemToMonkey id item = (monkey id . items) `modifying` (<> [item])

applyOperation :: Id -> Int -> State Env Int
applyOperation id item = do
  operation <- Maybe.fromJust <$> preuse (monkey id . operation)
  let
    value Old = item
    value (Value x) = x
  return $ case operation of
    Add x y -> value x + value y
    Multiply x y -> value x * value y

testItem :: Id -> Int -> State Env Id
testItem id item = do
  Test { _condition, _ifTrue, _ifFalse } <- Maybe.fromJust <$> preuse (monkey id . test)
  return . Id $ case item `mod` (_divisor _condition) of
    0 -> _ifTrue
    _ -> _ifFalse

throwItem :: Id -> Int -> State Env ()
throwItem id item = do
  r <- testItem id item
  giveItemToMonkey r item

incrementInspection :: Id -> State Env ()
incrementInspection id = (inspections . at (unId id) . _Just) `modifying` (+ 1)

monkeyDo :: (Int -> Int) -> Id -> State Env ()
monkeyDo worry id = do
  maybeItem <- takeItemFromMonkey id
  case maybeItem of
    Nothing -> return ()
    Just item -> do
      incrementInspection id
      item' <- applyOperation id item
      item'' <- moduloBase item'
      let item''' = worry item''
      throwItem id item'''
      monkeyDo worry id

run :: Int -> [Monkey] -> (Id -> State Env ()) -> Env
run n monkeys = (`State.execState` env) . forM_ (concat . replicate n $ map _id monkeys)
  where
    env = Env { _monkeys, _inspections, _base }
    _monkeys = IntMap.fromList [(id, monkey) | monkey <- monkeys, let Id id = _id monkey]
    _inspections = fmap (const 0) _monkeys
    _base = product $ map (view $ test . condition . divisor) monkeys

monkeyBusiness :: Env -> Int
monkeyBusiness = product . take 2 . reverse . List.sort . IntMap.elems . _inspections

part1 :: IO ()
part1 = do
  input <- readInput
  putStr "Part1: "
  print . monkeyBusiness $ run 20 input (monkeyDo (`div` 3))

part2 :: IO ()
part2 = do
  input <- readInput
  putStr "Part2: "
  print . monkeyBusiness $ run 10000 input (monkeyDo (\x -> x))

main :: IO ()
main = do
  part1 -- prints `Part1: 90294`
  part2 -- prints `Part2: 18170818354`
