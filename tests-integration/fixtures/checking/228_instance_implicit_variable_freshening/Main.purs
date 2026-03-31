module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

class Top a where
  top :: a

instance topProxy :: Top (Proxy a) where
  top = Proxy :: Proxy a
