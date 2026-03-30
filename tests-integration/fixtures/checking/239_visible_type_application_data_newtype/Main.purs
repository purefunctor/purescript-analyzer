module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

newtype Identity :: Type -> Type
newtype Identity a = Identity a

testProxy = Proxy @42
testIdentity = Proxy @Int
