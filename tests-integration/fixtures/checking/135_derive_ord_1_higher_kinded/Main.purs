module Main where

import Data.Eq (class Eq, class Eq1)
import Data.Ord (class Ord, class Ord1)

data Wrap f a = MkWrap (f a)

derive instance (Eq1 f, Eq a) => Eq (Wrap f a)
derive instance (Ord1 f, Ord a) => Ord (Wrap f a)

data WrapNoOrd1 f a = MkWrapNoOrd1 (f a)

derive instance (Eq1 f, Eq a) => Eq (WrapNoOrd1 f a)
derive instance Ord a => Ord (WrapNoOrd1 f a)
