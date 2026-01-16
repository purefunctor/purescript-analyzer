module Main where

import Data.Show (class Show)

newtype Vector (n :: Int) a = Vector (Array a)

derive newtype instance Show a => Show (Vector n a)
