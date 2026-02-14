module Main where

import Prim.Row as Row

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

foreign import unsafeCoerce :: forall a b. a -> b

openLeft :: forall r u. Row.Union (a :: Int | r) (b :: String) u => Proxy u
openLeft = Proxy

openRight :: forall r u. Row.Union (a :: Int) (b :: String | r) u => Proxy u
openRight = Proxy

backwardLeft :: forall l r. Row.Union l (b :: String) (a :: Int, b :: String | r) => Proxy l
backwardLeft = Proxy

backwardRight :: forall r u. Row.Union (a :: Int) r (a :: Int, b :: String | u) => Proxy r
backwardRight = Proxy

forceSolve = { openLeft, openRight, backwardLeft, backwardRight }
