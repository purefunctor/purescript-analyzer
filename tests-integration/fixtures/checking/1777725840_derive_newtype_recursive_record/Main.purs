module Main where

import Data.Eq (class Eq)

data Maybe a = Nothing | Just a

instance Eq a => Eq (Maybe a) where
  eq _ _ = true

newtype List a = List { head :: Int, tail :: Maybe (List a) }

derive newtype instance Eq a => Eq (List a)
