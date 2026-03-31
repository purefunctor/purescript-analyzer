module Main where

type Add a b = a

infixl 5 type Add as +

type Chain a b c = a + b + c

type ChainParen a b c = (a + b) + c
