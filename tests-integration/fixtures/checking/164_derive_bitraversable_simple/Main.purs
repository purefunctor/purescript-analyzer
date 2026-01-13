module Main where

import Data.Bitraversable (class Bitraversable)

data Either a b = Left a | Right b
derive instance Bitraversable Either

data Pair a b = Pair a b
derive instance Bitraversable Pair
