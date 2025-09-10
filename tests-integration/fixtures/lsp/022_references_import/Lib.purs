module Lib where

data Maybe a = Just a | Nothing

plus a b = 123

infix 5 plus as +

foreign import data Cons :: Type -> Type -> Type

infix 5 type Cons as :

class Eq
