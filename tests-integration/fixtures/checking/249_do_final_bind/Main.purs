module Main where

import Control.Applicative (pure)
import Control.Bind (bind)
import Effect (Effect)

test :: Effect Int
test = do
  x <- pure 1

test' = do
  x <- pure 1
