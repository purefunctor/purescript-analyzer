module Main where

type Add :: forall k. k -> k -> k
type Add a b = a

infixl 5 type Add as +

type Chain :: forall k. k -> k -> k -> k
type Chain a b c = a + b + c

type ChainParen :: forall k. k -> k -> k -> k
type ChainParen a b c = (a + b) + c
