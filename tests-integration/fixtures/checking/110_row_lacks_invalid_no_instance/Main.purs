module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: Row.Lacks "b" (a :: Int, b :: String) => Proxy (a :: Int, b :: String)
invalid = Proxy

forceSolve = { invalid }
