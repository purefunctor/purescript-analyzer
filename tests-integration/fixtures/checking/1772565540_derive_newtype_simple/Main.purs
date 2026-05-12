module Main where

import Data.Show (class Show)

newtype Wrapper = Wrapper Int

derive newtype instance Show Wrapper
