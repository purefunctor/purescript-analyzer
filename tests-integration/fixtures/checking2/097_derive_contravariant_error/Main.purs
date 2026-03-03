module Main where

import Data.Functor.Contravariant (class Contravariant)

data Identity a = Identity a
derive instance Contravariant Identity

data Producer a = Producer (Int -> a)
derive instance Contravariant Producer
