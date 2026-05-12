module Main where

import Data.Show (class Show)

newtype Identity a = Identity a

derive newtype instance Show (Identity String)
