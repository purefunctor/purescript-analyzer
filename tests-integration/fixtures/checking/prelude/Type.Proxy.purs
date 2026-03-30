module Type.Proxy where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy
