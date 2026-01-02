module Main where

import Prim.Int as Int
import Prim.Ordering (kind Ordering, LT)

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: Int.Compare 5 1 LT => Proxy LT
invalid = Proxy

forceSolve = { invalid }
