module Main where

import Data.Generic.Rep (class Generic)

data Box = Box Int

derive instance Generic Box _
