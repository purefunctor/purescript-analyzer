module Main where

import Data.Traversable (class Traversable)

data Identity a = Identity a
derive instance Traversable Identity

data Maybe a = Nothing | Just a
derive instance Traversable Maybe

data Const e a = Const e
derive instance Traversable (Const e)
