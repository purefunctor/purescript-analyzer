module Main where

import Data.Newtype (class Newtype)

newtype Wrapper a = Wrapper a

derive instance Newtype (Wrapper a) _
