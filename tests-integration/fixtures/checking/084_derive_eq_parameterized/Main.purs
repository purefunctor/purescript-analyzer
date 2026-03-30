module Main where

import Data.Eq (class Eq)

data Maybe a = Nothing | Just a

derive instance Eq a => Eq (Maybe a)
