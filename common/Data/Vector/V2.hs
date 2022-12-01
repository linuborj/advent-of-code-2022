{-# LANGUAGE DeriveGeneric #-}
module Data.Vector.V2 where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)


data V2 a = V2 a a
  deriving (Eq, Ord, Generic)

instance Hashable a => Hashable (V2 a)

instance Show a => Show (V2 a) where
  show (V2 x y) = "(" <> show x <> "," <> show y <> ")"

instance Foldable V2 where
  foldMap f (V2 x y) = f x <> f y

instance Functor V2 where
  fmap f (V2 x y)= V2 (f x) (f y)

instance Applicative V2 where
  pure x = V2 x x
  (V2 f g) <*> (V2 x y) = V2 (f x) (g y)

instance Traversable V2 where
  traverse f (V2 x y) = V2 <$> f x <*> f y

instance Num a => Num (V2 a) where
  u + v = (+) <$> u <*> v
  u - v = (-) <$> u <*> v
  u * v = (*) <$> u <*> v
  negate u = fmap negate u
  abs u = fmap abs u
  signum u = fmap signum u
  fromInteger x = V2 (fromInteger x) (fromInteger x)

scale :: Num a => V2 a -> a -> V2 a
scale u factor = fmap (* factor) u

manhattan :: Num a => V2 a -> a
manhattan = sum . abs

clockwise :: Num a => V2 a -> V2 a
clockwise (V2 x y) = V2 y (-x)

counterClockwise :: Num a => V2 a -> V2 a
counterClockwise (V2 x y) = V2 (-y) x

rotate :: Floating a => V2 a -> a -> V2 a
rotate (V2 x y) r = V2 (x * cos r - y * sin r) (x * sin r + y * cos r)

x, y :: V2 a -> a
x (V2 x y) = x
y (V2 x y) = y
