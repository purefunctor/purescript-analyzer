module Main where

import Prim.Int as Int

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: Int.ToString 42 "999" => Proxy "999"
invalid = Proxy

forceSolve = { invalid }
