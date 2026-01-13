module Main where

import Data.Functor (class Functor)

data Identity a = Identity a
derive instance Functor Identity

data Const e a = Const e
derive instance Functor (Const e)

data Maybe a = Nothing | Just a
derive instance Functor Maybe
