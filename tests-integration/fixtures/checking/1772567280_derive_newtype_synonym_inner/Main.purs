module Main where

import Data.Eq (class Eq)

foreign import data State :: Type

instance Eq State where
  eq _ _ = true

newtype Test = Test State

derive newtype instance Eq Test
