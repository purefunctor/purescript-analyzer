module Main where

import Data.Eq (class Eq)

data Either a b = Left a | Right b

derive instance Eq b => Eq (Either Int b)

derive instance Eq (Either Int b)
