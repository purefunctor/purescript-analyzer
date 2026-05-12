module Main where

import Data.Bifunctor (class Bifunctor)

data Either a b = Left a | Right b
derive instance Bifunctor Either

data Pair a b = Pair a b
derive instance Bifunctor Pair

data Const2 e a b = Const2 e
derive instance Bifunctor (Const2 e)
