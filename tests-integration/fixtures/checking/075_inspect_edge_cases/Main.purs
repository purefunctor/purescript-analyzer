module Main where

type JustInt = Int

justInt :: JustInt
justInt = 42

type NotAFunction = Int

notFunction :: forall a. a -> NotAFunction
notFunction a = 42

type Level1 = Int -> Int
type Level2 = Int -> Level1
type Level3 a = a -> Level2

deeply :: forall a. Level3 a
deeply a b c = c

type Inner = forall a. a -> a
type Outer = Inner

outer :: Outer
outer a = a

type Parens = (Int -> Int) -> Int

parens :: Parens
parens f = f 0

zeroArg :: forall a. a
zeroArg = zeroArg
