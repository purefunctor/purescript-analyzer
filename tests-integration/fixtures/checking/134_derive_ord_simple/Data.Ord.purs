module Data.Ord where

import Data.Eq (class Eq)

class Eq a <= Ord a where
  compare :: a -> a -> Int

instance Ord Int where
  compare _ _ = 0

instance Ord Boolean where
  compare _ _ = 0
