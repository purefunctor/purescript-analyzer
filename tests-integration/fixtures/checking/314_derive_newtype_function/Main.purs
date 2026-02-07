module Main where

import Data.Semigroupoid (class Semigroupoid)
import Control.Category (class Category)

newtype Builder a b = Builder (a -> b)

derive newtype instance Semigroupoid Builder
derive newtype instance Category Builder
