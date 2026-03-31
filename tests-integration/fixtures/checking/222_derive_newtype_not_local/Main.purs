module Main where

import Data.Show (class Show)
import Lib (Wrapper)

derive newtype instance Show Wrapper
