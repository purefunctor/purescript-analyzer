module Main where

import Data.Foldable (class Foldable)

data Identity a = Identity a
derive instance Foldable Identity

data Maybe a = Nothing | Just a
derive instance Foldable Maybe

data Const e a = Const e
derive instance Foldable (Const e)
