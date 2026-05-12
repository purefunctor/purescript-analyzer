module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

rightmostDuplicate :: forall l. Row.Union l (a :: String) (a :: Int, a :: String) => Proxy l
rightmostDuplicate = Proxy

leftmostDuplicateMismatch :: forall l. Row.Union l (a :: String) (a :: String, a :: Int) => Proxy l
leftmostDuplicateMismatch = Proxy

openOutputBackwards :: forall l r. Row.Union l (b :: String) (a :: Int, b :: String | r) => Proxy l
openOutputBackwards = Proxy

forceSolve =
  { rightmostDuplicate
  , leftmostDuplicateMismatch
  , openOutputBackwards
  }
