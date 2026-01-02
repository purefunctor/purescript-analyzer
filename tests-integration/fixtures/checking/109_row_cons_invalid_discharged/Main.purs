module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: forall t. Row.Cons "missing" Int t (a :: Int, b :: String) => Proxy t
invalid = Proxy

forceSolve = { invalid }
