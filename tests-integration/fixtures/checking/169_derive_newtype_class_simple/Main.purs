module Main where

import Data.Newtype (class Newtype)

newtype UserId = UserId Int

derive instance Newtype UserId _
