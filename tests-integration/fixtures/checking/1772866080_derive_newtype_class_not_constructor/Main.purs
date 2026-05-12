module Main where

import Data.Newtype (class Newtype)

derive instance Newtype (Int -> Int) _
