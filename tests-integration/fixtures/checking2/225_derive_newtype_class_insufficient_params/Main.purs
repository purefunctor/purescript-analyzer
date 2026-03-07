module Main where

import Data.Newtype (class Newtype)

newtype Wrapper = Wrapper Int

derive instance Newtype Wrapper
