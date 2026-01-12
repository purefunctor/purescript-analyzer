module Main where

import Data.Eq (class Eq)

data Box = MkBox Int

derive instance Eq Box
