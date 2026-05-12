module Main where

import Data.Foldable (class Foldable)
import Data.Bifoldable (class Bifoldable)

data WrapBoth f g a b = WrapBoth (f a) (g b)
derive instance (Foldable f, Foldable g) => Bifoldable (WrapBoth f g)

data WrapBothNoConstraint f g a b = WrapBothNoConstraint (f a) (g b)
derive instance Bifoldable (WrapBothNoConstraint f g)
