module Main where

import Data.Traversable (class Traversable)

data Compose f g a = Compose (f (g a))
derive instance (Traversable f, Traversable g) => Traversable (Compose f g)
