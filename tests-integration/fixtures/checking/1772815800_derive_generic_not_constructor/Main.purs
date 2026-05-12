module Main where

import Data.Generic.Rep (class Generic)

derive instance Generic (Int -> Int) _
