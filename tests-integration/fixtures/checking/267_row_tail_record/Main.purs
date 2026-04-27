module Main where

tail :: forall r. Record ( | r ) -> Int
tail _ = 42

nonTail :: forall r. Record r -> Int
nonTail _ = 42

asTail :: Array (forall r. Record ( | r ) -> Int)
asTail = [ tail, nonTail ]

asNonTail :: Array (forall r. Record r -> Int)
asNonTail = [ tail, nonTail ]
