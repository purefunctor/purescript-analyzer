module Main where

import Prim.Symbol as Symbol

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: forall s. Symbol.Append "hello" s "xyz" => Proxy s
invalid = Proxy

forceSolve = { invalid }
