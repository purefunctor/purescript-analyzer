module Main where

import Data.Show (class Show)

newtype Pair a b = Pair a

derive newtype instance Show (Pair Int String)
