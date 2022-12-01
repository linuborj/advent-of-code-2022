{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.Chart
  ( Chart
  , fromString
  , dimensions
  , (!)
  , (!?)
  , render
  , null
  , keys
  , elems
  , mapMaybe
  , member
  , fromList
  , toList
  , north
  , south
  , east
  , west
  , insert
  , delete
  , switch
  ) where

import Prelude hiding (null)
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Vector.V2 (V2 (..))
import qualified Data.Maybe as Maybe


newtype Chart a = Chart (HashMap (V2 Int) a)
  deriving (Eq, Ord, Functor, Semigroup, Monoid, Foldable, Generic)

instance Hashable a => Hashable (Chart a)

instance Show (Chart Char) where
  show = render (Maybe.fromMaybe ' ')

fromString :: String -> Chart Char
fromString = Chart . HashMap.fromList . concat . zipWith (\y -> zipWith (\x -> (V2 x y,)) [0..]) [0..] . lines

dimensions :: Chart a -> (V2 Int, V2 Int)
dimensions = foldl1 update . fmap tuple . keys
  where
    tuple x = (x, x)
    update (u, v) (z, w) = (min <$> u <*> z, max <$> v <*> w)

(!) :: Chart a -> V2 Int -> a
Chart cells ! u = cells HashMap.! u

(!?) :: Chart a -> V2 Int -> Maybe a
Chart cells !? u = cells HashMap.!? u

render :: (Maybe a -> Char) -> Chart a -> String
render f chart
  | null chart = ""
  | otherwise = unlines
    [ fmap (f . (chart !?) . (`V2` y)) [minX .. maxX]
    | y <- [minY .. maxY]
    ]
    where
      (V2 minX minY, V2 maxX maxY) = dimensions chart

null :: Chart a -> Bool
null (Chart cells) = HashMap.null cells

keys :: Chart a -> [V2 Int]
keys (Chart cells) = HashMap.keys cells

elems :: Chart a -> [a]
elems (Chart cells) = HashMap.elems cells

mapMaybe :: (a -> Maybe b) -> Chart a -> Chart b
mapMaybe f (Chart cells) = Chart (HashMap.mapMaybe f cells)

member :: V2 Int -> Chart a -> Bool
member u (Chart cells) = u `HashMap.member` cells

toList :: Chart a -> [(V2 Int, a)]
toList (Chart cells) = HashMap.toList cells

fromList :: [(V2 Int, a)] -> Chart a
fromList = Chart . HashMap.fromList

north, east, south, west :: V2 Int
north = V2 0 (-1)
east = V2 1 0
south = V2 0 1
west = V2 (-1) 0

insert :: V2 Int -> a -> Chart a -> Chart a
insert coordinate cell (Chart cells) = Chart $ HashMap.insert coordinate cell cells

delete :: V2 Int -> Chart a -> Chart a
delete coordinate (Chart cells) = Chart $ coordinate `HashMap.delete` cells

switch :: V2 Int -> V2 Int -> Chart a -> Chart a
switch first second chart = insert first (chart ! second) $ insert second (chart ! first) chart

