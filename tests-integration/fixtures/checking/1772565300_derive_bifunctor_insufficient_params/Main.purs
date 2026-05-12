module Main where

import Data.Bifunctor (class Bifunctor)

data Triple a b c = Triple a b c
derive instance Bifunctor (Triple Int String)
