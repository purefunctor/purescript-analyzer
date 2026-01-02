module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: forall l. Row.Union l (b :: String) (a :: Int, b :: Int) => Proxy l
invalid = Proxy

forceSolve = { invalid }
