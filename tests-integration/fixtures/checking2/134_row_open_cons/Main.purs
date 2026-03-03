module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

consOpen :: forall r row. Row.Cons "x" Int (a :: String | r) row => Proxy row
consOpen = Proxy

decomposeOpen :: forall t tail r. Row.Cons "x" t tail (x :: Int, a :: String | r) => Proxy t
decomposeOpen = Proxy

extractTail :: forall tail r. Row.Cons "x" Int tail (x :: Int, a :: String | r) => Proxy tail
extractTail = Proxy

forceSolve = { consOpen, decomposeOpen, extractTail }
