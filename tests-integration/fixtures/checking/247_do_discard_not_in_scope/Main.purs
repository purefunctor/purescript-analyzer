module Main where

import Control.Applicative (pure)
import Control.Bind (bind)
import Effect (Effect)

test :: Effect Int
test = do
  pure 0
  x <- pure 1
  pure x

test' = do
  pure 0
  x <- pure 1
  pure x
