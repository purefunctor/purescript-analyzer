module Main where

class Phantom a where
  identity :: a -> a

data Proxy a = Proxy

instance Phantom (Proxy a) where
  identity a = a

forceSolve = { solution: identity Proxy }
