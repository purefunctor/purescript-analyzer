module Main where

import Data.Functor (class Functor)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)

data Compose f g a = Compose (f (g a))
derive instance (Functor f, Functor g) => Functor (Compose f g)
derive instance (Foldable f, Foldable g) => Foldable (Compose f g)
derive instance (Traversable f, Traversable g) => Traversable (Compose f g)
