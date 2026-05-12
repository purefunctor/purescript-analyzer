module Main where

import Data.Eq (class Eq, class Eq1)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord, class Ord1)

newtype App :: forall k. (k -> Type) -> k -> Type
newtype App f a = App (f a)

derive instance Newtype (App f a) _
derive instance (Eq1 f, Eq a) => Eq (App f a)
derive instance Eq1 f => Eq1 (App f)
derive instance (Ord1 f, Ord a) => Ord (App f a)
derive instance Ord1 f => Ord1 (App f)
