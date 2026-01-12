module Main where

import Data.Eq (class Eq)
import Data.Ord (class Ord)

data Box = MkBox Int

derive instance Eq Box
derive instance Ord Box

data Pair = MkPair Int Boolean

derive instance Eq Pair
derive instance Ord Pair

data NoOrd = MkNoOrd

derive instance Eq NoOrd

data ContainsNoOrd = MkContainsNoOrd NoOrd

derive instance Eq ContainsNoOrd
derive instance Ord ContainsNoOrd
