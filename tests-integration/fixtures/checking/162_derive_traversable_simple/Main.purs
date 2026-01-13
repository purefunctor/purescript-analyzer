module Main where

import Data.Functor (class Functor)
import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)

data Identity a = Identity a
derive instance Functor Identity
derive instance Foldable Identity
derive instance Traversable Identity

data Maybe a = Nothing | Just a
derive instance Functor Maybe
derive instance Foldable Maybe
derive instance Traversable Maybe

data Const e a = Const e
derive instance Functor (Const e)
derive instance Foldable (Const e)
derive instance Traversable (Const e)
