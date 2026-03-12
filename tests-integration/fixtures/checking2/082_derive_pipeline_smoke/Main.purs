module Main where

import Data.Eq (class Eq, eq)

data Box = Box

derive instance Eq Box

test :: Boolean
test = eq Box Box
