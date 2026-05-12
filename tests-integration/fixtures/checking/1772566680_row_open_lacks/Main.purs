module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

lacksOpen :: forall r. Row.Lacks "missing" (a :: Int, b :: String | r) => Proxy r -> Int
lacksOpen _ = 0

lacksPresent :: forall r. Row.Lacks "a" (a :: Int | r) => Proxy r -> Int
lacksPresent _ = 0

empty :: Proxy ()
empty = Proxy

forceSolve = { lacksOpen: lacksOpen empty, lacksPresent: lacksPresent empty }
