module Lib where

life :: Int
life = 42

data Maybe a = Just a | Nothing

plus :: Int -> Int -> Int
plus _ _ = 0

infix 5 plus as +

foreign import data Cons :: Type -> Type -> Type

infix 5 type Cons as ++

class ClassName a
