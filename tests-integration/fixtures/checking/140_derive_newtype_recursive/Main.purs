module Main where

import Data.Show (class Show)

newtype Mu f = Mu (f (Mu f))

derive newtype instance Show (f (Mu f)) => Show (Mu f)
