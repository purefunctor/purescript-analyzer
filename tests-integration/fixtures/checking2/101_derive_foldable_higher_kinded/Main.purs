module Main where

import Data.Foldable (class Foldable)

data Wrap f a = Wrap (f a)
derive instance Foldable f => Foldable (Wrap f)

data WrapNoFoldable f a = WrapNoFoldable (f a)
derive instance Foldable (WrapNoFoldable f)
