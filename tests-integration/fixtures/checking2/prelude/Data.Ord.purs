module Data.Ord where

import Data.Eq (class Eq, class Eq1)

data Ordering = LT | EQ | GT

class Eq a <= Ord a where
  compare :: a -> a -> Ordering

class Eq1 f <= Ord1 f where
  compare1 :: forall a. Ord a => f a -> f a -> Ordering

instance Ord Int where
  compare _ _ = EQ

instance Ord Boolean where
  compare _ _ = EQ
