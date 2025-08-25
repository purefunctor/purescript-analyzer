module Main where

type P :: forall k. k -> Type
type P (a :: k) = Proxy a
