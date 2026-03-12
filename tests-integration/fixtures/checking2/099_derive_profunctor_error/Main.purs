module Main where

import Data.Profunctor (class Profunctor)

data WrongFirst a b = WrongFirst a b
derive instance Profunctor WrongFirst

data WrongSecond a b = WrongSecond (b -> a)
derive instance Profunctor WrongSecond
