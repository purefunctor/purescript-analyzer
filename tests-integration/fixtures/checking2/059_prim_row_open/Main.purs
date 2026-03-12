module Main where

import Prim.Row as Row
import Type.Proxy (Proxy(..))

openLeft :: forall r u. Row.Union (a :: Int | r) (b :: String) u => Proxy u
openLeft = Proxy

openRight :: forall r u. Row.Union (a :: Int) (b :: String | r) u => Proxy u
openRight = Proxy

backwardLeft :: forall l r. Row.Union l (b :: String) (a :: Int, b :: String | r) => Proxy l
backwardLeft = Proxy

backwardRight :: forall r u. Row.Union (a :: Int) r (a :: Int, b :: String | u) => Proxy r
backwardRight = Proxy

consOpen :: forall r row. Row.Cons "x" Int (a :: String | r) row => Proxy row
consOpen = Proxy

decomposeOpen :: forall t tail r. Row.Cons "x" t tail (x :: Int, a :: String | r) => Proxy t
decomposeOpen = Proxy

extractTail :: forall tail r. Row.Cons "x" Int tail (x :: Int, a :: String | r) => Proxy tail
extractTail = Proxy

lacksOpen :: forall r. Row.Lacks "missing" (a :: Int, b :: String | r) => Proxy r -> Int
lacksOpen _ = 0

lacksPresent :: forall r. Row.Lacks "a" (a :: Int | r) => Proxy r -> Int
lacksPresent _ = 0

forceSolve =
  { openLeft
  , openRight
  , backwardLeft
  , backwardRight
  , consOpen
  , decomposeOpen
  , extractTail
  , lacksOpen: lacksOpen Proxy
  , lacksPresent: lacksPresent Proxy
  }
