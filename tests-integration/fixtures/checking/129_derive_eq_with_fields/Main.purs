module Main where

import Data.Eq (class Eq)

data Box = MkBox Int

derive instance Eq Box

data Pair = MkPair Int Boolean

derive instance Eq Pair
