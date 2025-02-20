module Main where

data Maybe a = Just a | Nothing

data Either a b = Left a | Right b

data Proxy :: forall k. k -> Type
data Proxy (a :: k) = Proxy

data Set a

type role Set nominal
type role Maybe representational
type role Proxy phantom
