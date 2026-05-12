module Main where

import Data.Bifoldable (class Bifoldable)

data Either a b = Left a | Right b
derive instance Bifoldable Either

data Pair a b = Pair a b
derive instance Bifoldable Pair

data Const2 e a b = Const2 e
derive instance Bifoldable (Const2 e)
