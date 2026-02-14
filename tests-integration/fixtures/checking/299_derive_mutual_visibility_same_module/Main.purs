module Main where

import Data.Eq (class Eq)

data DurationComponent = Hours | Minutes | Seconds

data Duration = Duration DurationComponent Int

-- Eq Duration depends on Eq DurationComponent, which is derived later.
derive instance Eq Duration
derive instance Eq DurationComponent
