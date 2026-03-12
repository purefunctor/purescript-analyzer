module Main where

import Data.Newtype (class Newtype)
import Lib (Wrapper)

derive instance Newtype Wrapper _
