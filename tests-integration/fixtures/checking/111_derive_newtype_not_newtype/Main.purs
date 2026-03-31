module Main where

import Data.Show (class Show)

data Foo = Foo Int

derive newtype instance Show Foo
