module Main where

newtype Id a = Id a

newtype P :: forall k. k -> Type
newtype P (a :: k) = P (Proxy a)
