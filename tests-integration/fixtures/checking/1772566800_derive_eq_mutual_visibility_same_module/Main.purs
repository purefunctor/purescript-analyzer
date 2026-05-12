module Main where

import Data.Eq (class Eq)

data DurationComponent = Hours | Minutes | Seconds

data Duration = Duration DurationComponent Int

derive instance Eq Duration
derive instance Eq DurationComponent
