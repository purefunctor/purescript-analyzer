module Main where

import Control.Category (class Category)
import Data.Semigroupoid (class Semigroupoid)

newtype Builder a b = Builder (a -> b)

derive newtype instance Semigroupoid Builder
derive newtype instance Category Builder
