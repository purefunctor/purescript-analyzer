module Main where

import Data.Bifunctor (class Bifunctor)

data WrapBoth f g a b = WrapBoth (f a) (g b)
derive instance Bifunctor (WrapBoth f g)
