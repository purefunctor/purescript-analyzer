module Main where

data Identity :: forall k. (k -> k) -> Type
data Identity a = Identity

foreign import fn :: forall k. Identity k -> Identity k

-- Foreign values must be generalised immediately
-- to avoid solving them to concrete types on usage.

test = fn (Identity :: Identity Array)
