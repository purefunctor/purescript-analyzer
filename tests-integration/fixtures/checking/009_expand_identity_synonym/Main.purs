module Main where

type Identity a = a

foreign import data Digit :: Int

type Test1 = Identity Int
type Test2 = Identity Digit
