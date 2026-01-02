module Main where

import Prim.Int as Int

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: Int.Mul 2 3 10 => Proxy 10
invalid = Proxy

forceSolve = { invalid }
