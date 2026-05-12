module Main where

import Data.Newtype (class Newtype)

data NotANewtype = NotANewtype Int

derive instance Newtype NotANewtype _
