module Main where

import Data.Functor.Contravariant (class Contravariant)

-- Should fail: a appears covariantly (directly in constructor)
data Identity a = Identity a
derive instance Contravariant Identity

-- Should fail: a appears covariantly (in result of function)
data Producer a = Producer (Int -> a)
derive instance Contravariant Producer
