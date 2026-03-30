module Main where

import Data.Bifunctor (class Bifunctor)
import Data.Functor (class Functor)

data WrapBoth f g a b = WrapBoth (f a) (g b)
derive instance (Functor f, Functor g) => Bifunctor (WrapBoth f g)

data WrapBothNoConstraint f g a b = WrapBothNoConstraint (f a) (g b)
derive instance Bifunctor (WrapBothNoConstraint f g)
