module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

tail :: forall r. Proxy ( | r ) -> Int
tail _ = 42

nonTail :: forall r. Proxy r -> Int
nonTail _ = 42

asTail :: Array (forall r. Proxy ( | r ) -> Int)
asTail = [ tail, nonTail ]

asNonTail :: Array (forall r. Proxy r -> Int)
asNonTail = [ tail, nonTail ]
