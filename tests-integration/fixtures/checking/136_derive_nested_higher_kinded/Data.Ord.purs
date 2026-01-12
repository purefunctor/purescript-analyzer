module Data.Ord where

import Data.Eq (class Eq, class Eq1)

class Eq a <= Ord a where
  compare :: a -> a -> Int

class Eq1 f <= Ord1 f where
  compare1 :: forall a. Ord a => f a -> f a -> Int

instance Ord Int where
  compare _ _ = 0
