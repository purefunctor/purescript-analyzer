module Main where

import Prim.Symbol as Symbol

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

invalid :: forall t. Symbol.Cons "hello" t "helloworld" => Proxy t
invalid = Proxy

forceSolve = { invalid }
