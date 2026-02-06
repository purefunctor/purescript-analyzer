module Main where

import Data.Newtype (class Newtype, wrap)

newtype Age = Age Int

derive instance Newtype Age _

wrapAge :: Int -> Age
wrapAge = wrap
