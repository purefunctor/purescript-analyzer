module Main where

type Add a b = a

infixl 5 type Add as :+:

type Mul a b = a

infixl 6 type Mul as :*:

type Chain a b c = a :+: b :*: c

type ChainParen a b c = (a :+: b) :*: c
