module Main where

data Proxy :: forall k. k -> Type
data Proxy a = Proxy

type Test0 = Proxy

test0 :: Test0 42
test0 = Proxy

type Test1 a = Proxy

test1 :: Test1 Int 42
test1 = Proxy

type Test2 a b = Proxy

test2 :: Test2 Int String 42
test2 = Proxy
