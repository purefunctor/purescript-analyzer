module Main where

import Data.Profunctor (class Profunctor)

-- Should fail: first param (a) appears covariantly
data WrongFirst a b = WrongFirst a b
derive instance Profunctor WrongFirst

-- Should fail: second param (b) appears contravariantly
data WrongSecond a b = WrongSecond (b -> a)
derive instance Profunctor WrongSecond
