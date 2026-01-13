module Main where

import Data.Traversable (class Traversable)
import Data.Bitraversable (class Bitraversable)

data WrapBoth f g a b = WrapBoth (f a) (g b)
derive instance (Traversable f, Traversable g) => Bitraversable (WrapBoth f g)
