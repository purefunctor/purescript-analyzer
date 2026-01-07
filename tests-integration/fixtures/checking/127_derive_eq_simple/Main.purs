module Main where

import Data.Eq (class Eq)

data Proxy a = Proxy

derive instance Eq (Proxy a)
